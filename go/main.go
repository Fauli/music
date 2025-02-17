package main

import (
	"flag"
	"fmt"
	"math"
	"math/rand"
	"os"
	"sort"
	"time"

	"github.com/faiface/beep"
	"github.com/faiface/beep/wav"
)

const (
	sampleRate    = 44100 // Audio sample rate.
	bpm           = 80    // Beats per minute.
	midiBeatTicks = 480   // Ticks per beat in MIDI.
)

// =======================
// MIDI Generation Helpers
// =======================

// midiEvent represents a MIDI event with an absolute tick value.
type midiEvent struct {
	tick int    // Absolute tick time.
	data []byte // Event data bytes.
}

// encodeVarLen encodes an integer as a MIDI variable-length quantity.
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

// saveMIDI creates a minimal Type‑0 MIDI file that fills the track for durationSec seconds.
// It repeats the drum pattern and chord progression, adding random velocity variation,
// hi‐hat skips, and sometimes arpeggiated chords.
func saveMIDI(filename string, durationSec float64, drumPattern []uint8, chords [][]uint8) error {
	// Calculate total number of beats in the track.
	totalBeats := int(durationSec * float64(bpm) / 60.0)
	var events []midiEvent

	// Add a tempo meta event at tick 0.
	// For 80 BPM: 60/80 * 1e6 = 750000 microseconds per quarter note.
	events = append(events, midiEvent{
		tick: 0,
		data: []byte{0xFF, 0x51, 0x03, 0x0B, 0x71, 0xB0},
	})

	// --- Drum Events ---
	// For each beat, use the drumPattern (assumed to be 16 steps) in round-robin.
	for beat := 0; beat < totalBeats; beat++ {
		tick := beat * midiBeatTicks
		note := drumPattern[beat%len(drumPattern)]
		if note != 0 {
			var velocity uint8
			switch note {
			case 36: // Kick: velocity between 90-110.
				velocity = uint8(90 + rand.Intn(21))
			case 38: // Snare: velocity between 80-100.
				velocity = uint8(80 + rand.Intn(21))
			case 42: // Hi-hat: sometimes skip for variety.
				if rand.Float64() < 0.3 {
					continue
				}
				velocity = uint8(70 + rand.Intn(31))
			default:
				velocity = 100
			}
			// Channel 10: Note-on is 0x99 and note-off is 0x89.
			events = append(events, midiEvent{
				tick: tick,
				data: []byte{0x99, note, velocity},
			})
			events = append(events, midiEvent{
				tick: tick + 240,
				data: []byte{0x89, note, 0},
			})
		}
	}

	// --- Chord Events ---
	// Each chord lasts 4 beats.
	for chordStartBeat := 0; chordStartBeat < totalBeats; chordStartBeat += 4 {
		chord := chords[(chordStartBeat/4)%len(chords)]
		chordStartTick := chordStartBeat * midiBeatTicks
		chordEndTick := (chordStartBeat + 4) * midiBeatTicks
		// Randomly decide to arpeggiate (50% chance) or play as block chord.
		if rand.Float64() < 0.5 {
			deltaIncrement := 30 // ticks offset between notes
			currentTick := chordStartTick
			for _, note := range chord {
				vel := uint8(70 + rand.Intn(21))
				events = append(events, midiEvent{
					tick: currentTick,
					data: []byte{0x90, note, vel},
				})
				currentTick += deltaIncrement
			}
			currentTick = chordEndTick
			for _, note := range chord {
				events = append(events, midiEvent{
					tick: currentTick,
					data: []byte{0x80, note, 0},
				})
				currentTick += deltaIncrement
			}
		} else {
			for _, note := range chord {
				vel := uint8(70 + rand.Intn(21))
				events = append(events, midiEvent{
					tick: chordStartTick,
					data: []byte{0x90, note, vel},
				})
			}
			for _, note := range chord {
				events = append(events, midiEvent{
					tick: chordEndTick,
					data: []byte{0x80, note, 0},
				})
			}
		}
	}

	// End-of-track meta event.
	events = append(events, midiEvent{
		tick: totalBeats * midiBeatTicks,
		data: []byte{0xFF, 0x2F, 0x00},
	})

	// Sort events by tick.
	sort.Slice(events, func(i, j int) bool {
		return events[i].tick < events[j].tick
	})

	// Convert absolute tick values to delta times.
	var trackData []byte
	prevTick := 0
	for _, ev := range events {
		delta := ev.tick - prevTick
		prevTick = ev.tick
		trackData = append(trackData, encodeVarLen(delta)...)
		trackData = append(trackData, ev.data...)
	}

	// Build MIDI file.
	var midiFile []byte
	// MIDI header: "MThd", length 6, format 0, 1 track, division 480.
	header := []byte{
		'M', 'T', 'h', 'd',
		0, 0, 0, 6,
		0, 0, // Format 0.
		0, 1, // One track.
		0x01, 0xE0, // Division: 480 ticks.
	}
	midiFile = append(midiFile, header...)

	// Track chunk.
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

	return os.WriteFile(filename, midiFile, 0o644)
}

// =======================
// Pattern Generation
// =======================

// generateDrumPattern returns a fixed 16‑step pattern.
// (The variety is added later in scheduling and audio synthesis.)
func generateDrumPattern() []uint8 {
	pattern := make([]uint8, 16)
	for i := 0; i < 16; i++ {
		if i%4 == 0 {
			pattern[i] = 36 // Kick.
		} else if i%8 == 4 {
			pattern[i] = 38 // Snare.
		} else if i%2 == 1 {
			pattern[i] = 42 // Hi-hat.
		} else {
			pattern[i] = 0
		}
	}
	return pattern
}

// generateChordProgression returns a simple chord progression.
func generateChordProgression() [][]uint8 {
	return [][]uint8{
		{60, 64, 67}, // C major.
		{62, 65, 69}, // D minor.
		{67, 71, 74}, // G major.
		{60, 67, 72}, // C major variation.
	}
}

// =======================
// Audio (WAV) Synthesis
// =======================

// synthesizeBeat generates a stereo sample buffer for durationSec seconds,
// adding random variations in envelope decay and amplitude for drums and chords.
func synthesizeBeat(durationSec float64, drumPattern []uint8, chords [][]uint8) [][2]float64 {
	totalSamples := sampleRate * int(durationSec)
	buffer := make([][2]float64, totalSamples)
	beatDurationSec := 60.0 / float64(bpm)
	beatSamples := int(beatDurationSec * float64(sampleRate))
	numBeats := totalSamples / beatSamples

	// --- Drum Synthesis ---
	for i := 0; i < numBeats; i++ {
		start := i * beatSamples
		note := drumPattern[i%len(drumPattern)]
		if note != 0 {
			// Vary overall amplitude for this drum hit.
			ampFactor := 0.8 + rand.Float64()*0.4 // between 0.8 and 1.2
			switch note {
			case 36: // Kick.
				dur := int(0.2 * float64(sampleRate))
				decay := 10 + rand.Float64()*5 // vary decay factor.
				for j := 0; j < dur && start+j < totalSamples; j++ {
					t := float64(j) / float64(sampleRate)
					env := math.Exp(-t * decay)
					sample := math.Sin(2*math.Pi*60*t) * env * ampFactor
					buffer[start+j][0] += sample
					buffer[start+j][1] += sample
				}
			case 38: // Snare.
				dur := int(0.2 * float64(sampleRate))
				decay := 15 + rand.Float64()*5
				for j := 0; j < dur && start+j < totalSamples; j++ {
					t := float64(j) / float64(sampleRate)
					env := math.Exp(-t * decay)
					sample := (((rand.Float64() * 2) - 1) * env * ampFactor)
					buffer[start+j][0] += sample
					buffer[start+j][1] += sample
				}
			case 42: // Hi-hat.
				if rand.Float64() < 0.3 {
					continue // sometimes skip hi-hat.
				}
				dur := int(0.1 * float64(sampleRate))
				decay := 30 + rand.Float64()*10
				for j := 0; j < dur && start+j < totalSamples; j++ {
					t := float64(j) / float64(sampleRate)
					env := math.Exp(-t * decay)
					sample := (((rand.Float64() * 2) - 1) * env * ampFactor * 0.8)
					buffer[start+j][0] += sample
					buffer[start+j][1] += sample
				}
			}
		}
	}

	// --- Chord Synthesis ---
	// Schedule chords every 4 beats.
	for chordStartBeat := 0; chordStartBeat < numBeats; chordStartBeat += 4 {
		chord := chords[(chordStartBeat/4)%len(chords)]
		startSample := chordStartBeat * beatSamples
		endSample := (chordStartBeat + 4) * beatSamples
		if endSample > totalSamples {
			endSample = totalSamples
		}
		chordAmp := 0.18 + rand.Float64()*0.1 // vary chord amplitude.
		// Randomly decide to arpeggiate.
		arpeggiate := rand.Float64() < 0.5
		if arpeggiate {
			// Each note starts with a small random offset.
			for _, note := range chord {
				freq := 440.0 * math.Pow(2, float64(note-69)/12.0)
				offset := rand.Intn(beatSamples / 8)
				for i := startSample + offset; i < endSample; i++ {
					t := float64(i-(startSample+offset)) / float64(beatSamples)
					var env float64
					if t < 0.1 {
						env = t / 0.1
					} else {
						env = math.Exp(-(t - 0.1) * (1.5 + rand.Float64()*0.5))
					}
					sample := math.Sin(2*math.Pi*freq*t) * env * chordAmp
					buffer[i][0] += sample
					buffer[i][1] += sample
				}
			}
		} else {
			// Block chord.
			for _, note := range chord {
				freq := 440.0 * math.Pow(2, float64(note-69)/12.0)
				for i := startSample; i < endSample; i++ {
					t := float64(i-startSample) / float64(beatSamples)
					var env float64
					if t < 0.1 {
						env = t / 0.1
					} else {
						env = math.Exp(-(t - 0.1) * (1.5 + rand.Float64()*0.5))
					}
					sample := math.Sin(2*math.Pi*freq*t) * env * chordAmp
					buffer[i][0] += sample
					buffer[i][1] += sample
				}
			}
		}
	}

	return buffer
}

// sliceStreamer is a simple implementation of beep.Streamer that streams a slice of stereo samples.
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

// saveWAV synthesizes the audio beat and writes it as a WAV file.
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

// =======================
// Main
// =======================

func main() {
	// Define a command-line flag to set the track duration.
	durationPtr := flag.Float64("duration", 10.0, "Duration of the track in seconds")
	flag.Parse()
	duration := *durationPtr

	rand.Seed(time.Now().UnixNano())

	// Generate file names based on the current date/time.
	now := time.Now()
	timestamp := now.Format("2006-01-02_15-04-05")
	midiFilename := fmt.Sprintf("lofi_beat_%s.mid", timestamp)
	wavFilename := fmt.Sprintf("lofi_beat_%s.wav", timestamp)

	drumPattern := generateDrumPattern()
	chords := generateChordProgression()

	// Save MIDI file.
	if err := saveMIDI(midiFilename, duration, drumPattern, chords); err != nil {
		fmt.Println("Error saving MIDI:", err)
	} else {
		fmt.Printf("MIDI file saved as %s\n", midiFilename)
	}

	// Save WAV file.
	if err := saveWAV(wavFilename, duration, drumPattern, chords); err != nil {
		fmt.Println("Error saving WAV:", err)
	} else {
		fmt.Printf("WAV file saved as %s\n", wavFilename)
	}
}

