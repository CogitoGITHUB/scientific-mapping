#!/usr/bin/env nu

# literature-review.nu - Automate literature review processes
# Usage: ./literature-review.nu "topic" [--max-papers N] [--since date] [--until date] [--output file.org]

def main [
  topic: string                        # Research topic to review
  --max-papers: int = 20              # Maximum papers to include
  --since: string = "2020-01-01"      # Start date
  --until: string = (date now | format date "%Y-%m-%d")  # End date
  --output: string = ""               # Output file (auto-generated if empty)
] {
  let output_file = if $output == "" {
    $"literature-review-($topic | str replace " " "-" | str downcase)-($until).org"
  } else {
    $output
  }

  print $"Generating literature review for '($topic)'"
  print $"Date range: ($since) to ($until)"
  print $"Maximum papers: ($max_papers)"
  print $"Output: ($output_file)"

  # Simulate querying citation database for topic
  # In real implementation, this would search the actual scientific-mapping database
  let mock_papers = [
    {
      title: $"Advances in ($topic) Research"
      authors: ["Researcher, A.", "Scientist, B."]
      year: 2023
      doi: "10.1000/adv-2023-001"
      journal: "Journal of Advanced Research"
      citations: 25
      abstract: $"This paper presents recent advances in ($topic), focusing on novel methodologies and their applications."
      keywords: [$topic, "methodology", "applications"]
      relevance_score: 0.95
    }
    {
      title: $"Theoretical Foundations of ($topic)"
      authors: ["Theorist, C.", "Academic, D."]
      year: 2022
      doi: "10.1000/theory-2022-002"
      journal: "Theoretical Science Review"
      citations: 45
      abstract: $"A comprehensive theoretical analysis of ($topic) with mathematical foundations and proofs."
      keywords: [$topic, "theory", "mathematics"]
      relevance_score: 0.88
    }
    {
      title: $"Practical Applications of ($topic)"
      authors: ["Practitioner, E."]
      year: 2024
      doi: "10.1000/practice-2024-003"
      journal: "Applied Science Journal"
      citations: 12
      abstract: $"Real-world applications and case studies demonstrating ($topic) in practice."
      keywords: [$topic, "applications", "case studies"]
      relevance_score: 0.92
    }
  ]

  # Filter by date and limit results
  let filtered_papers = $mock_papers |
    where {|paper| $paper.year >= ($since | str substring 0..3 | into int)} |
    where {|paper| $paper.year <= ($until | str substring 0..3 | into int)} |
    sort-by relevance_score | reverse |
    first $max_papers

  # Generate Org-mode literature review
  let org_content = $"
#+TITLE: Literature Review: ($topic)
#+AUTHOR: Scientific Mapping System
#+DATE: (date now | format date "%Y-%m-%d")
#+OPTIONS: toc:t num:t

* Executive Summary

This literature review covers ($topic), analyzing ($filtered_papers | length) key papers
published between ($since) and ($until). The review identifies current trends,
methodological approaches, and research gaps in the field.

* Methodology

Papers were selected based on relevance to ($topic), citation impact, and publication date.
The review focuses on high-impact research that advances understanding of ($topic).

* Key Findings

** Publication Trends
- Total papers analyzed: ($filtered_papers | length)
- Average citations per paper: (($filtered_papers | get citations | math sum) / ($filtered_papers | length) | math round 1)
- Most active year: ($filtered_papers | get year | uniq | sort | last)

** Research Themes
($filtered_papers | get keywords | flatten | group-by | transpose key value | each {|row| $"- ($row.key): ($row.value | length) papers"} | str join "\n")

* Individual Paper Analysis
"

  # Add individual paper sections
  let paper_sections = $filtered_papers | each {|paper|
    $"
** ($paper.title)
   :PROPERTIES:
   :DOI: ($paper.doi)
   :YEAR: ($paper.year)
   :CITATIONS: ($paper.citations)
   :RELEVANCE: ($paper.relevance_score)
   :END:

*** Authors
($paper.authors | each {|author| $"- ($author)"} | str join "\n")

*** Abstract
($paper.abstract)

*** Key Contributions
- [ ] Identify main contributions
- [ ] Assess methodology quality
- [ ] Evaluate impact on field

*** Citations and References
- Published in: ($paper.journal) ($paper.year)
- DOI: [[doi:($paper.doi)][($paper.doi)]]
- Citations: ($paper.citations)

*** Notes
- Relevance score: ($paper.relevance_score)
- Keywords: ($paper.keywords | str join ", ")

*** Follow-up Questions
- [ ] What are the limitations of this approach?
- [ ] How does this relate to other work in ($topic)?
- [ ] What future research directions does this suggest?
"
  } | str join "\n"

  # Add conclusion and research gaps
  let conclusion = $"
* Research Gaps and Future Directions

** Identified Gaps
- [ ] Integration with other methodologies
- [ ] Scalability for large-scale applications
- [ ] Validation across diverse domains

** Future Research Directions
- [ ] Hybrid approaches combining multiple methods
- [ ] Real-time and streaming applications
- [ ] Interdisciplinary applications

* Conclusion

This literature review provides a comprehensive analysis of current research in ($topic).
The field shows significant progress with ($filtered_papers | length) key publications
contributing to our understanding. However, several research gaps remain that warrant
further investigation.

* References
"

  # Combine all content
  let full_content = $org_content + $paper_sections + $conclusion

  # Save to file
  $full_content | save $output_file
  print $"Literature review saved to ($output_file)"

  # Print summary statistics
  print $"\n=== Literature Review Summary ==="
  print $"Topic: ($topic)"
  print $"Papers analyzed: ($filtered_papers | length)"
  print $"Date range: ($since) - ($until)"
  print $"Total citations: ($filtered_papers | get citations | math sum)"
  print $"Average relevance: (($filtered_papers | get relevance_score | math sum) / ($filtered_papers | length) | math round 2)"

  print $"\nTop keywords:"
  $filtered_papers | get keywords | flatten | group-by | transpose key value |
    each {|row| $"  ($row.key): ($row.value | length) occurrences"} | first 5 | each {print $in}

  $filtered_papers
}