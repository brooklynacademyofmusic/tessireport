# write.attendance_report assembles pdf output from process.attendance_report

    Code
      mock_args(write_pdf)[1]
    Output
      [[1]]
      [[1]][[1]]
      
      \begin{longtable}{>{\raggedleft\arraybackslash}p{0.75in}>{\raggedright\arraybackslash}p{0.75in}>{\raggedright\arraybackslash}p{1.75in}>{\raggedright\arraybackslash}p{1in}>{\raggedright\arraybackslash}p{1in}>{\raggedright\arraybackslash}p{1in}>{\raggedright\arraybackslash}p{1in}>{\raggedright\arraybackslash}p{1in}lll}
      \toprule
      group\_customer\_no & date & id & name & sort\_name & perf\_desc & perf\_dt & status & ship\_method & seats & source\\
      \midrule
      \endfirsthead
      \multicolumn{11}{@{}l}{\textit{(continued)}}\\
      \toprule
      group\_customer\_no & date & id & name & sort\_name & perf\_desc & perf\_dt & status & ship\_method & seats & source\\
      \midrule
      \endhead
      
      \endfoot
      \bottomrule
      \endlastfoot
      \addlinespace[0.3em]
      \multicolumn{11}{l}{\textbf{2025-04-26}}\\
      \hspace{1em}\cellcolor{gray!10}{1} & \cellcolor{gray!10}{2025-04-26} & \cellcolor{gray!10}{15708658} & \cellcolor{gray!10}{Customer 1} & \cellcolor{gray!10}{1/Customer} & \cellcolor{gray!10}{Sinners} & \cellcolor{gray!10}{Apr 26 07:30 PM} & \cellcolor{gray!10}{Ticketed, Paid} & \cellcolor{gray!10}{Mobile Tickets} & \cellcolor{gray!10}{GenA: 214} & \cellcolor{gray!10}{orders}\\
      \hspace{1em}13 & 2025-04-26 & 15712457 & Customer 13 & 13/Customer & Sinners & Apr 26 04:30 PM & Ticketed, Paid & Mobile Tickets & GenA: 29 & orders\\
      \hspace{1em}\cellcolor{gray!10}{36} & \cellcolor{gray!10}{2025-04-26} & \cellcolor{gray!10}{15712077} & \cellcolor{gray!10}{Customer 36} & \cellcolor{gray!10}{36/Customer} & \cellcolor{gray!10}{Sinners} & \cellcolor{gray!10}{Apr 26 07:30 PM} & \cellcolor{gray!10}{Ticketed, Paid} & \cellcolor{gray!10}{Mobile Tickets} & \cellcolor{gray!10}{GenA: 218} & \cellcolor{gray!10}{orders}\\
      \hspace{1em}5 & 2025-04-26 & 15707494 & Customer 5 & 5/Customer & Sinners & Apr 26 07:30 PM & Ticketed, Paid & Mobile Tickets & GenA: 22 & orders\\
      \hspace{1em}\cellcolor{gray!10}{74} & \cellcolor{gray!10}{2025-04-26} & \cellcolor{gray!10}{15702741} & \cellcolor{gray!10}{Customer 74} & \cellcolor{gray!10}{74/Customer} & \cellcolor{gray!10}{Sinners} & \cellcolor{gray!10}{Apr 26 04:30 PM} & \cellcolor{gray!10}{Ticketed, Paid} & \cellcolor{gray!10}{Mobile Tickets} & \cellcolor{gray!10}{GenA: 14} & \cellcolor{gray!10}{orders}\\
      \addlinespace[0.3em]
      \multicolumn{11}{l}{\textbf{2025-04-27}}\\
      \hspace{1em}1 & 2025-04-27 & 15705061 & Customer 1 & 1/Customer & Sinners (Open Caption) & Apr 27 07:30 PM & Ticketed, Paid & Mobile Tickets & GenA: 14 & orders\\
      \hspace{1em}\cellcolor{gray!10}{22} & \cellcolor{gray!10}{2025-04-27} & \cellcolor{gray!10}{15710092} & \cellcolor{gray!10}{Customer 22} & \cellcolor{gray!10}{22/Customer} & \cellcolor{gray!10}{Sinners} & \cellcolor{gray!10}{Apr 27 01:30 PM} & \cellcolor{gray!10}{Ticketed, Paid} & \cellcolor{gray!10}{Mobile Tickets} & \cellcolor{gray!10}{GenA: 14} & \cellcolor{gray!10}{orders}\\
      \hspace{1em}48 & 2025-04-27 & 15705589 & Customer 48 & 48/Customer & Legend of Ochi (Open Caption) & Apr 27 04:20 PM & Ticketed, Paid & Mobile Tickets & GenA: 11 & orders\\
      \hspace{1em}\cellcolor{gray!10}{6} & \cellcolor{gray!10}{2025-04-27} & \cellcolor{gray!10}{15710136} & \cellcolor{gray!10}{Customer 6} & \cellcolor{gray!10}{6/Customer} & \cellcolor{gray!10}{The Shrouds} & \cellcolor{gray!10}{Apr 27 04:10 PM} & \cellcolor{gray!10}{Ticketed, Paid} & \cellcolor{gray!10}{Mobile Tickets} & \cellcolor{gray!10}{GenA: 15} & \cellcolor{gray!10}{orders}\\
      \hspace{1em}63 & 2025-04-27 & 15706265 & Customer 63 & 63/Customer & The Shrouds & Apr 27 04:10 PM & Ticketed, Paid & Mobile Tickets & GenA: 11 & orders\\*
      \end{longtable}
      
      

