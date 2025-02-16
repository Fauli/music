package main

import (
	"fmt"
	"math"
	"math/rand"
	"os"
	"sort"
	"time"

	"github.com/faiface/beep"
	"github.com/faiface/beep/wav"
)

// ----- MIDI File Generation -----
//
// We will write a minimal Type‑0 MIDI file manually.
// The file will have one track containing a tempo event,
// a 16‑step drum loop (using channel 10, i.e. status bytes 0x99/0x89),
// and a 4‑chord progression on channel 1 (status 0x90/0x80).

const (
	sampleRate = 44100 // for audio synthesis
	bpm        = 80
	// In MIDI, we use 480 ticks per beat.
	midiBeatTicks = 480
)

// midiEvent represents a MIDI event with an absolute tick and its data bytes.
type midiEvent struct {
	tick int
	data []byte
}

// encodeVarLen encodes an integer as a variable-length quantity.
func encodeVarLen(value int) []byte {
	var buffer []byte
	buffer = append(buffer, byte(value&0x7F))
	value >>= 7
	for value > 0 {
		b := byte((value & 0x7F) | 0x80)
		buffer = append([]byte{b}, buffer...)
		value >>= 7
	}
	return buffer
}

// saveMIDI creates a MIDI file named filename.
func saveMIDI(filename string, drumPattern []uint8, chords [][]uint8) error {
	var events []midiEvent

	// Tempo meta event at tick 0.
	// 80 BPM => 60/80 * 1e6 = 750000 microseconds per quarter note.
	events = append(events, midiEvent{
		tick: 0,
		data: []byte{0xFF, 0x51, 0x03, 0x0B, 0x71, 0xB0},
	})

	// Drum loop: use 16 beats (each beat = 480 ticks).
	// For each beat, if drumPattern[i] is nonzero, add a Note On
	// at the beat start and a Note Off 240 ticks later.
	for i := 0; i < 16; i++ {
		tick := i * midiBeatTicks
		note := drumPattern[i%len(drumPattern)]
		if note != 0 {
			// Drum channel: channel 10 is 0x99 for note-on, 0x89 for note-off.
			events = append(events, midiEvent{
				tick: tick,
				data: []byte{0x99, note, 100},
			})
			events = append(events, midiEvent{
				tick: tick + 240,
				data: []byte{0x89, note, 0},
			})
		}
	}

	// Chord progression: assume 4 chords, each lasting 4 beats (4*480 = 1920 ticks).
	for j, chord := range chords {
		startTick := j * 1920
		endTick := startTick + 1920
		for _, note := range chord {
			// Channel 1: 0x90 for note on, 0x80 for note off.
			events = append(events, midiEvent{
				tick: startTick,
				data: []byte{0x90, note, 80},
			})
			events = append(events, midiEvent{
				tick: endTick,
				data: []byte{0x80, note, 0},
			})
		}
	}

	// End-of-track meta event at tick 16*480.
	events = append(events, midiEvent{
		tick: 16 * midiBeatTicks,
		data: []byte{0xFF, 0x2F, 0x00},
	})

	// Sort events by their absolute tick.
	sort.Slice(events, func(i, j int) bool {
		return events[i].tick < events[j].tick
	})

	// Convert absolute ticks to delta times and build track data.
	var trackData []byte
	prevTick := 0
	for _, ev := range events {
		delta := ev.tick - prevTick
		prevTick = ev.tick
		trackData = append(trackData, encodeVarLen(delta)...)
		trackData = append(trackData, ev.data...)
	}

	// Build the MIDI file.
	var midiFile []byte

	// MIDI header chunk: "MThd", length 6, format 0, 1 track, division = 480.
	header := []byte{
		'M', 'T', 'h', 'd',
		0, 0, 0, 6,
		0, 0, // format 0
		0, 1, // one track
		0x01, 0xE0, // division: 480 ticks (0x01E0)
	}
	midiFile = append(midiFile, header...)

	// Track chunk header.
	trackHeader := []byte{'M', 'T', 'r', 'k'}
	midiFile = append(midiFile, trackHeader...)
	trackLen := uint32(len(trackData))
	midiFile = append(midiFile,
		byte(trackLen>>24),
		byte(trackLen>>16),
		byte(trackLen>>8),
		byte(trackLen),
	)
	midiFile = append(midiFile, trackData...)

	// Write the complete MIDI file.
	return os.WriteFile(filename, midiFile, 0o644)
}

// generateDrumPattern creates a 16‑step drum pattern.
// It places a kick (36) on every 4th step,
// a snare (38) on step 5 (i%8==4),
// and a hi‑hat (42) on every odd step.
func generateDrumPattern() []uint8 {
	pattern := make([]uint8, 16)
	for i := 0; i < 16; i++ {
		if i%4 == 0 {
			pattern[i] = 36 // Kick
		} else if i%8 == 4 {
			pattern[i] = 38 // Snare
		} else if i%2 == 1 {
			pattern[i] = 42 // Hi-hat
		} else {
			pattern[i] = 0
		}
	}
	return pattern
}

// generateChordProgression returns a simple chord progression.
func generateChordProgression() [][]uint8 {
	return [][]uint8{
		{60, 64, 67}, // C major
		{62, 65, 69}, // D minor
		{67, 71, 74}, // G major
		{60, 67, 72}, // C major variation
	}
}

// ----- WAV File (Audio) Synthesis -----
//
// We “synthesize” a 10‑second stereo beat by
// adding drum sounds and chord sounds into a sample buffer.

// synthesizeBeat generates a stereo buffer (as a slice of [2]float64)
// of length (durationSec * sampleRate).
func synthesizeBeat(durationSec float64, drumPattern []uint8, chords [][]uint8) [][2]float64 {
	totalSamples := sampleRate * int(durationSec)
	buffer := make([][2]float64, totalSamples)

	// Calculate beat duration in seconds from BPM.
	beatDurationSec := 60.0 / float64(bpm) // e.g. 0.75 sec at 80 BPM
	beatSamples := int(beatDurationSec * float64(sampleRate))
	numBeats := totalSamples / beatSamples

	// --- Drum synthesis ---
	for i := 0; i < numBeats; i++ {
		start := i * beatSamples
		note := drumPattern[i%len(drumPattern)]
		if note != 0 {
			if note == 36 { // Kick: low-frequency sine with fast decay.
				dur := int(0.2 * float64(sampleRate))
				for j := 0; j < dur && start+j < totalSamples; j++ {
					t := float64(j) / float64(sampleRate)
					env := math.Exp(-t * 10) // fast decay
					sample := math.Sin(2*math.Pi*60*t) * env * 0.8
					buffer[start+j][0] += sample
					buffer[start+j][1] += sample
				}
			} else if note == 38 { // Snare: noise burst.
				dur := int(0.2 * float64(sampleRate))
				for j := 0; j < dur && start+j < totalSamples; j++ {
					t := float64(j) / float64(sampleRate)
					env := math.Exp(-t * 15)
					sample := ((rand.Float64() * 2) - 1) * env * 0.5
					buffer[start+j][0] += sample
					buffer[start+j][1] += sample
				}
			} else if note == 42 { // Hi-hat: shorter noise burst.
				dur := int(0.1 * float64(sampleRate))
				for j := 0; j < dur && start+j < totalSamples; j++ {
					t := float64(j) / float64(sampleRate)
					env := math.Exp(-t * 30)
					sample := ((rand.Float64() * 2) - 1) * env * 0.3
					buffer[start+j][0] += sample
					buffer[start+j][1] += sample
				}
			}
		}
	}

	// --- Chord synthesis ---
	// We schedule chords for the first four chords.
	// Each chord lasts 4 beats.
	chordDurationSamples := beatSamples * 4
	for j, chord := range chords {
		start := j * chordDurationSamples
		end := start + chordDurationSamples
		if start >= totalSamples {
			break
		}
		if end > totalSamples {
			end = totalSamples
		}
		// For each note in the chord, add a sine wave.
		for _, note := range chord {
			// Convert MIDI note number to frequency.
			freq := 440.0 * math.Pow(2, float64(note-69)/12.0)
			for i := start; i < end; i++ {
				t := float64(i-start) / float64(sampleRate)
				// Use a soft envelope: quick attack then exponential decay.
				var env float64
				if t < 0.1 {
					env = t / 0.1
				} else {
					env = math.Exp(-(t - 0.1) * 1.5)
				}
				sample := math.Sin(2*math.Pi*freq*t) * env * 0.2
				buffer[i][0] += sample
				buffer[i][1] += sample
			}
		}
	}

	return buffer
}

// sliceStreamer is a simple implementation of beep.Streamer that streams
// a slice of stereo samples.
type sliceStreamer struct {
	buf [][2]float64
	pos int
}

func (s *sliceStreamer) Stream(samples [][2]float64) (n int, ok bool) {
	if s.pos >= len(s.buf) {
		return 0, false
	}
	n = copy(samples, s.buf[s.pos:])
	s.pos += n
	return n, true
}

func (s *sliceStreamer) Err() error {
	return nil
}

// saveWAV synthesizes the beat and writes it as a WAV file.
func saveWAV(filename string, durationSec float64, drumPattern []uint8, chords [][]uint8) error {
	samples := synthesizeBeat(durationSec, drumPattern, chords)
	streamer := &sliceStreamer{buf: samples}
	format := beep.Format{
		SampleRate:  sampleRate,
		NumChannels: 2,
		Precision:   2,
	}
	f, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer f.Close()
	return wav.Encode(f, streamer, format)
}

// ----- main -----
func main() {
	rand.Seed(time.Now().UnixNano())

	// Generate our musical patterns.
	drumPattern := generateDrumPattern()
	chords := generateChordProgression()
	duration := 10.0 // seconds

	// Save MIDI file.
	if err := saveMIDI("lofi_beat.mid", drumPattern, chords); err != nil {
		fmt.Println("Error saving MIDI:", err)
	} else {
		fmt.Println("MIDI file saved as lofi_beat.mid")
	}

	// Save WAV file.
	if err := saveWAV("lofi_beat.wav", duration, drumPattern, chords); err != nil {
		fmt.Println("Error saving WAV:", err)
	} else {
		fmt.Println("WAV file saved as lofi_beat.wav")
	}
}

