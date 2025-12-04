# Business Writing Skill

**Version**: 1.0.0
**Created**: October 27, 2025
**Author**: RAI B2B Atlas + FLOW
**Status**: Production Ready

---

## Purpose

Automatically applies Sid's business writing voice when composing B2B sales emails, professional correspondence, or business communication.

**Core principles:**
- Simple, direct, human (never salesy)
- 2-5 sentences ideal
- Grand Slam Offer strategy (make offers irresistible)
- 6 questions framework (clarity before writing)

---

## Auto-Activation

This skill activates when Claude detects:
- Writing B2B sales emails
- Follow-up after demo/discovery call
- Pricing proposals or offers
- Deal check-ins or status updates
- Professional business correspondence
- Objection handling emails

---

## Core Protocol

**Phase 0 → Phase N (7 phases total)**

1. **Phase 0**: Context detection (follow-up, offer, check-in, etc.)
2. **Phase 1**: Load resources (6 questions, Grand Slam, examples, voice guide)
3. **Phase 2**: Answer 6 questions (clarity framework)
4. **Phase 3**: Apply strategy (4 value drivers if B2B sales)
5. **Phase 4**: Write in Sid's voice (simple, direct, brief)
6. **Phase 5**: Anti-pattern check (remove sales fluff)
7. **Phase N**: Show 2-3 versions for choice

---

## Directory Structure

```
business-writing/
├── SKILL.md              # Core protocol (you're using this)
├── README.md             # This file
├── references/
│   ├── clear-writing.md  # 6 questions framework
│   ├── grand-slam.md     # 4 value drivers strategy
│   └── b2b-emails.md     # Real examples (Gun, YD Builders)
└── assets/
    ├── anti-patterns.md  # Salesy language to avoid
    └── voice-guide.md    # Sid's voice patterns
```

---

## Usage Examples

### Example 1: Follow-up After Demo

**Input**: "Write follow-up email to Gun after demo"

**Skill activates:**
1. Detects: Follow-up context
2. Loads: 6 questions + voice guide + b2b-emails.md
3. Applies: Clarity + Sid's voice (not Grand Slam needed)
4. Output: 3-sentence check-in

**Result**:
```
Hey Gun,

You good to start this week?

Let me know if anything's blocking you.

- Sid
```

### Example 2: Pricing Offer Email

**Input**: "Draft pricing offer for YD Builders"

**Skill activates:**
1. Detects: Offer context (B2B sales)
2. Loads: ALL resources (6 questions + Grand Slam + examples + voice)
3. Applies: Full strategy (4 value drivers)
4. Output: Partnership-angle offer with fast win

**Result**: Gun Belgium email pattern (see references/b2b-emails.md)

### Example 3: Professional Non-Sales

**Input**: "Email to Ercan about meeting reschedule"

**Skill activates:**
1. Detects: Professional (not sales)
2. Loads: 6 questions + voice guide only
3. Applies: Clarity + brevity
4. Output: Direct, simple request

---

## Key Differentiators

**What makes this different from generic business writing:**

1. **Sid's voice embedded** - Not corporate speak
2. **Grand Slam strategy** - Make offers irresistible (when needed)
3. **Anti-pattern enforcement** - Catches salesy language
4. **Progressive disclosure** - Loads only what's needed for context
5. **Real examples** - Actual emails that closed deals

---

## Success Metrics

**Email quality indicators:**
- ✅ 2-5 sentences (10 max)
- ✅ One clear point/action
- ✅ Sounds like Sid (colleague test: "Would Sid write this?")
- ✅ No sales fluff ("excited to share", "reaching out")
- ✅ Reader knows exactly what to do next

**Deal metrics (B2B sales):**
- ✅ Offer feels irresistible
- ✅ 4 value drivers applied
- ✅ Fast win emphasized
- ✅ Friction removed

---

## When NOT to Use

**Skip this skill for:**
- Internal team Slack messages
- Personal emails to friends/family
- Social media posts (use `/context-writing` instead)
- Blog articles or long-form content
- Technical documentation

---

## Related Skills

- **claude-code-platform**: Platform knowledge
- **garden-protocol**: Journal tending

**Related commands:**
- `/context-writing`: For social media, articles, long-form
- `/context-logseq`: For Logseq operations

---

## Changelog

### v1.0.0 (Oct 27, 2025)
- Initial release
- Phase 0 → N protocol established
- 6 questions framework integrated
- Grand Slam Offer strategy embedded
- Real B2B email examples (Gun, YD Builders)
- Anti-pattern detection added
- Sid's voice patterns codified

---

## Future Enhancements

**Planned for v1.1:**
- Objection handling templates
- Partnership angle variations
- Follow-up timing strategies
- A/B tested subject lines

**Feedback welcome**: Update based on real usage patterns

---

*"Make it so simple they can't say no. Make it so brief they actually read it."*
