000100 Id Division.                                                             
000200     skip2                                                                
000300 Program-Id.     WDMR1800.                                                
000400*AUTHOR.         ODD OLSEN.                                               
000500*DATE-WRITTEN.   92/01/22.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPA LADDFIL FÖR DATA MANAGER                                   
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     skip3                                                                
001900 Environment Division.                                                    
002000     skip2                                                                
002100 Input-Output Section.                                                    
002200                                                                          
002300 File-Control.                                                            
002400     skip2                                                                
002500*          --- PROCEDURER                                                 
002600     Select WDMR11                     Assign To WDMR18D1.                
002700     skip2                                                                
002800*          --- JOB-STEG                                                   
002900     Select WDMR12                     Assign To WDMR18D2.                
003000     skip2                                                                
003100*          --- PROGRAM                                                    
003200     Select WDMR13                     Assign To WDMR18D3.                
003300     skip2                                                                
003400*          --- DATASET                                                    
003500     Select WDMR14                     Assign To WDMR18D4.                
003600     skip2                                                                
003700*          --- PROCEDURER                                                 
003800     Select WDMR1F                     Assign To WDMR18D5.                
003900     skip2                                                                
004000*          --- JOB-STEG                                                   
004100     Select WDMR1G                     Assign To WDMR18D6.                
004200     skip2                                                                
004300*          --- PROGRAM                                                    
004400     Select WDMR1H                     Assign To WDMR18D7.                
004500     skip2                                                                
004600*          --- DATASET                                                    
004700     Select WDMR1I                     Assign To WDMR18D8.                
004800     eject                                                                
004900 Data Division.                                                           
005000     skip3                                                                
005100 File Section.                                                            
005200     skip3                                                                
005300 Fd  WDMR11                                                               
005400     Recording       F                                                    
005500     Block Contains  0.                                                   
005600     skip2                                                                
005700 01  Filler                   Pic X(81).                                  
005800     skip3                                                                
005900 Fd  WDMR12                                                               
006000     Recording       F                                                    
006100     Block Contains  0.                                                   
006200     skip2                                                                
006300 01  Filler                   Pic X(81).                                  
006400     skip3                                                                
006500 Fd  WDMR13                                                               
006600     Recording       F                                                    
006700     Block Contains  0.                                                   
006800     skip2                                                                
006900 01  Filler                   Pic X(96).                                  
007000     skip3                                                                
007100 Fd  WDMR14                                                               
007200     Recording       F                                                    
007300     Block Contains  0.                                                   
007400     skip2                                                                
007500 01  Filler                   Pic X(81).                                  
007600     skip3                                                                
007700 Fd  WDMR1F                                                               
007800     Recording       F                                                    
007900     Block Contains  0.                                                   
008000     skip2                                                                
008100 01  proc-post                Pic X(80).                                  
008200     skip3                                                                
008300 Fd  WDMR1G                                                               
008400     Recording       F                                                    
008500     Block Contains  0.                                                   
008600     skip2                                                                
008700 01  job-post                 Pic X(80).                                  
008800     skip3                                                                
008900 Fd  WDMR1H                                                               
009000     Recording       F                                                    
009100     Block Contains  0.                                                   
009200     skip2                                                                
009300 01  pgm-post                 Pic X(80).                                  
009400     skip3                                                                
009500 Fd  WDMR1I                                                               
009600     Recording       F                                                    
009700     Block Contains  0.                                                   
009800     skip2                                                                
009900 01  dsn-post                 Pic X(80).                                  
010000     eject                                                                
010100 Working-Storage Section.                                                 
010101                                                                          
010110*    -- CHECKED BY WY2000                                                 
010200     skip2                                                                
010300 77  idpgm                       Pic X(8)    Value 'WDMR1800'.            
010400 77  ja                          Pic X       Value 'J'.                   
010500 77  nej                         Pic X       Value 'N'.                   
010600 77  fnutt                       Pic X       Value ''''.                  
010700 77  w-str                       Pic X(200).                              
010710 77  w-cat-word                  Pic X(10).                               
010800 77  end-punkt                   Pic X       Value 'N'.                   
010900 77  skrivit-contains            Pic X       Value 'N'.                   
011000 77  rub-ix                      Pic 9(4)    Binary.                      
011100 77  pekare                      Pic 9(4)    Binary.                      
011200                                                                          
011300 77  spar-proc                   Pic X(13)   Value Space.                 
011400 77  spar-job                    Pic X(17)   Value Space.                 
011500 77  spar-pgm                    Pic X(22)   Value Space.                 
011600 77  spar-dsn                    Pic X(41)   Value Space.                 
011700     eject                                                                
011800* EOF flaggor                                                             
011900                                                                          
012000 77  rdmr11-eof-sw               Pic X       Value 'N'.                   
012100     88  end-of-rdmr11                       Value 'J'.                   
012200                                                                          
012300 77  rdmr12-eof-sw               Pic X       Value 'N'.                   
012400     88  end-of-rdmr12                       Value 'J'.                   
012500                                                                          
012600 77  rdmr13-eof-sw               Pic X       Value 'N'.                   
012700     88  end-of-rdmr13                       Value 'J'.                   
012800                                                                          
012900 77  rdmr14-eof-sw               Pic X       Value 'N'.                   
013000     88  end-of-rdmr14                       Value 'J'.                   
013100     eject                                                                
013200 01  dagens-datum                Pic 9(6)    Value Zero.                  
013300 01  Filler Redefines dagens-datum.                                       
013400     03  dagens-datum-aar        Pic 9(2).                                
013500     03  dagens-datum-maanad     Pic 9(2).                                
013600     03  dagens-datum-dag        Pic 9(2).                                
013700     skip3                                                                
013800 01  dynamiska-subprogram.                                                
013900*                                                                         
014000     03  abend                   Pic X(8)    Value 'ABEND'.               
014100     03  postsum                 Pic X(8)    Value 'POSTSUM'.             
014200                                                                          
014300*    --- PARAMETRAR TILL ABEND                                            
014400                                                                          
014500 77  rkod-abend-utan-dump        Pic S9(4)   Comp Value +16.              
014600 77  rkod-abend-med-dump         Pic S9(4)   Comp Value +1000.            
014700     skip2                                                                
014800 01  feltext.                                                             
014900     03  Filler                  Pic X(8)    Value 'FELTEXT'.             
015000     03  feltext-str             Pic X(72)   Value Space.                 
015100     eject                                                                
015200*    --- Arbetsareor för redigering av utfiler                            
015300 01  command.                                                             
015400     03                          Pic X(08) Value 'REPLACE'.               
015500     03 cmd-member               Pic X(50).                               
015600     03                          Pic X(01) Value '.'.                     
015700 01  rubriker.                                                            
015800     03 rub-proc                 Pic X(60) Value 'PROCEDURE'.             
015900     03 rub-job                  Pic X(60) Value 'JOB-STEP'.              
016000     03 rub-pgm                  Pic X(60) Value 'PROGRAM'.               
016100     03 rub-dsn                  Pic X(60) Value 'DATASET'.               
016200     03 rub-catalog              Pic X(60) Value 'CATALOG'.               
016300     03 rub-contains             Pic X(60) Value 'CONTAINS'.              
016400 01  cat-names.                                                           
016500     03                          Pic X(04) Value Space.                   
016600     03 cat-name-area            Pic X(70).                               
016700 01  contains-post.                                                       
016800     03                          Pic X(04) Value Space.                   
016900     03 contains-komma           Pic X(01) Value Space.                   
017000     03 contains-mbr             Pic X(50) Value Space.                   
017100 01  obsolete                    Pic X(50)                                
017200        Value 'OBSOLETE-DEF CATALOG ''REMOVE'' '.                         
017210 01  cmd-remove.                                                          
017220     03                          Pic X(08) Value 'REMOVE'.                
017230     03 rem-member               Pic X(50).                               
017240     03                          Pic X(01) Value ' '.                     
017300     eject                                                                
017400*    --- PARAMETRAR TILL POSTSUM                                          
017500*                                                                         
017600*01  -COPY W0005   -PRE  POSTSUM-                                         
017700     eject                                                                
017800 01  proc-area-start             Pic X(24)   Value                        
017900                                 'PROC-AREA-START  '.                     
018000                                                                          
018100 01  in-proc-area.                                                        
018200     03 proc-styr                Pic X(01).                               
018300     03 proc-procedur            Pic X(13).                               
018400     03 proc-steg                Pic X(17).                               
018500     03 proc-program             Pic X(22).                               
018600                                                                          
018700     skip3                                                                
018800 01  job-area-start              Pic X(24)   Value                        
018900                                 'JOB-AREA-START  '.                      
019000                                                                          
019100 01  in-job-area.                                                         
019200     03 job-styr                 Pic X(01).                               
019300     03 job-steg                 Pic X(17).                               
019400     03 job-program              Pic X(22).                               
019500                                                                          
019600     eject                                                                
019700 01  pgm-area-start              Pic X(24)   Value                        
019800                                 'PGM-AREA-START  '.                      
019900 01  in-pgm-area.                                                         
020000     03 pgm-styr                 Pic X(01).                               
020100     03 pgm-program              Pic X(22).                               
020200     03 pgm-pgmtyp               Pic X(08).                               
020300     03 pgm-psb                  Pic X(12).                               
020400     03 pgm-dsnamn               Pic X(41).                               
020410     03 pgm-laddmodul            Pic X(12).                               
020500                                                                          
020600     skip3                                                                
020700 01  dsn-area-start              Pic X(24)   Value                        
020800                                 'DSN-AREA-START  '.                      
020900 01  in-dsn-area.                                                         
021000     03 dsn-styr                 Pic X(01).                               
021100     03 dsn-dsnamn               Pic X(41).                               
021200     03 dsn-ddnamn               Pic X(21).                               
021300                                                                          
021400     eject                                                                
021500 01  utproc-area-start           Pic X(24)   Value                        
021600                                 'UTPROC-AREA-START  '.                   
021700 01  ut-proc-area                Pic X(80).                               
021800                                                                          
021900     eject                                                                
022000 01  utjob-area-start            Pic X(24)   Value                        
022100                                 'UTJOB-AREA-START  '.                    
022200 01  ut-job-area                 Pic X(80).                               
022300                                                                          
022400     eject                                                                
022500 01  utpgm-area-start            Pic X(24)   Value                        
022600                                 'UTPGM-AREA-START  '.                    
022700 01  ut-pgm-area                 Pic X(80).                               
022800                                                                          
022900     eject                                                                
023000 01  utdsn-area-start            Pic X(24)   Value                        
023100                                 'UTDSN-AREA-START  '.                    
023200 01  ut-dsn-area                 Pic X(80).                               
023300                                                                          
023400     eject                                                                
023500 Linkage Section.                                                         
023600                                                                          
023700 01  parm.                                                                
023800     03  parm-laengd             Pic S9(4)   Comp Sync.                   
023900     03  parm-vaerde.                                                     
024000       05 skriv-proc             Pic X(1).                                
024100       05 skriv-steg             Pic X(1).                                
024200       05 skriv-pgm              Pic X(1).                                
024300       05 skriv-dsn              Pic X(1).                                
024400     eject                                                                
024500 Procedure Division Using parm.                                           
024600     skip2                                                                
024700 STYR Section.                                                            
024800                                                                          
024900     Perform A-INIT                                                       
025000     If skriv-proc = ja                                                   
025100       Perform B-SKAPA-PROC                                               
025200     End-If                                                               
025300     If skriv-steg = ja                                                   
025400       Perform C-SKAPA-STEG                                               
025500     End-If                                                               
025600     If skriv-pgm  = ja                                                   
025700       Perform D-SKAPA-PGM                                                
025800     End-If                                                               
025900     If skriv-dsn  = ja                                                   
026000       Perform E-SKAPA-DSN                                                
026100     End-If                                                               
026200                                                                          
026300     Perform Z-FINIT                                                      
026400                                                                          
026500     Move Zero To return-code                                             
026600     Goback                                                               
026700     .                                                                    
026800     eject                                                                
026900 A-INIT Section.                                                          
027000                                                                          
027100     Open Input                                                           
027200       WDMR11 WDMR12 WDMR13 WDMR14                                        
027300                                                                          
027400     Open Output                                                          
027500       WDMR1F WDMR1G WDMR1H WDMR1I                                        
027600     skip2                                                                
027700     Accept dagens-datum  From Date                                       
027800     Move idpgm To postsum-prognamn                                       
027900     .                                                                    
028000     eject                                                                
028100 B-SKAPA-PROC Section.                                                    
028200                                                                          
028300     Perform S01-LAES-WDMR11                                              
028400     Perform Until end-of-rdmr11                                          
028500       If spar-proc Not = proc-procedur                                   
028600         Move nej To skrivit-contains                                     
028700         If end-punkt = nej                                               
028800           Move ja To end-punkt                                           
028900         Else                                                             
029000           Move '.' To ut-proc-area                                       
029100           Perform S11-SKRIV-WDMR1F                                       
029200         End-if                                                           
029300         Move proc-procedur To cmd-member spar-proc rem-member            
029400         If proc-styr Not = 'D'                                           
029500           Perform BA-FIRST-NEW-RCD                                       
029600         Else                                                             
029700           Perform BC-DELETE-RCD                                          
029800         End-If                                                           
029900       Else                                                               
030000         Perform BB-NEXT-NEW-RCD                                          
030100       End-If                                                             
030200                                                                          
030300       Perform S01-LAES-WDMR11                                            
030400     End-Perform                                                          
030500     Move '.' To ut-proc-area                                             
030600     Perform S11-SKRIV-WDMR1F                                             
030700     .                                                                    
030800     eject                                                                
030900 BA-FIRST-NEW-RCD Section.                                                
031000     Move command         To ut-proc-area                                 
031100     Perform S11-SKRIV-WDMR1F                                             
031200     Move rub-proc        To ut-proc-area                                 
031300     Perform S11-SKRIV-WDMR1F                                             
031400     Move Space           To ut-proc-area                                 
031500     Perform S11-SKRIV-WDMR1F                                             
031600     Move rub-catalog     To ut-proc-area                                 
031700     Perform S11-SKRIV-WDMR1F                                             
031800     Perform BAA-KATALOGORD                                               
031900     Perform S11-SKRIV-WDMR1F                                             
032000     If proc-steg = Space And proc-program = Space                        
032100       Continue                                                           
032200     Else                                                                 
032300       Move rub-contains   To ut-proc-area                                
032400       Perform S11-SKRIV-WDMR1F                                           
032500       Move ja To skrivit-contains                                        
032600       Move Space          To contains-komma                              
032700       If proc-program Not = Space                                        
032800         Move proc-program   To contains-mbr                              
032900       Else                                                               
033000         Move proc-steg      To contains-mbr                              
033100       End-If                                                             
033200       Move contains-post  To ut-proc-area                                
033300       Perform S11-SKRIV-WDMR1F                                           
033400     End-If                                                               
033500     .                                                                    
033600     eject                                                                
033700 BAA-KATALOGORD Section.                                                  
033800                                                                          
033900     Move 1     To pekare                                                 
034000     Move Space To cat-name-area                                          
034100     String fnutt Delimited By Size                                       
034200            rub-proc  Delimited By Space                                  
034300            fnutt     Delimited By Size                                   
034400       Into cat-name-area                                                 
034500       With Pointer pekare                                                
034600     Move cat-names To ut-proc-area                                       
034700     .                                                                    
034800     eject                                                                
034900 BB-NEXT-NEW-RCD Section.                                                 
035000     If skrivit-contains = nej                                            
035100       Move rub-contains   To ut-proc-area                                
035200       Perform S11-SKRIV-WDMR1F                                           
035300       Move ja To skrivit-contains                                        
035400       Move Space To contains-komma                                       
035500     Else                                                                 
035600       Move ','   To contains-komma                                       
035700     End-If                                                               
035800     If proc-program Not = Space                                          
035900       Move proc-program To contains-mbr                                  
036000     Else                                                                 
036100       Move proc-steg    To contains-mbr                                  
036200     End-If                                                               
036300     Move contains-post To ut-proc-area                                   
036400     Perform S11-SKRIV-WDMR1F                                             
036500     .                                                                    
036600     eject                                                                
036700 BC-DELETE-RCD Section.                                                   
036800     Move cmd-remove      To ut-proc-area                                 
036900     Perform S11-SKRIV-WDMR1F                                             
037200     .                                                                    
037300     eject                                                                
037400 C-SKAPA-STEG Section.                                                    
037500                                                                          
037600     Move nej To end-punkt                                                
037700     Perform S02-LAES-WDMR12                                              
037800     Perform Until end-of-rdmr12                                          
037900       If spar-job  Not = job-steg                                        
038000         Move nej To skrivit-contains                                     
038100         If end-punkt = nej                                               
038200           Move ja To end-punkt                                           
038300         Else                                                             
038400           Move '.' To ut-job-area                                        
038500           Perform S12-SKRIV-WDMR1G                                       
038600         End-if                                                           
038700         Move job-steg      To cmd-member spar-job rem-member             
038800         If job-styr Not = 'D'                                            
038900           Perform CA-FIRST-NEW-RCD                                       
039000         Else                                                             
039100           Perform CC-DELETE-RCD                                          
039200         End-If                                                           
039300       Else                                                               
039400         Perform CB-NEXT-NEW-RCD                                          
039500       End-If                                                             
039600                                                                          
039700       Perform S02-LAES-WDMR12                                            
039800     End-Perform                                                          
039900     Move '.' To ut-job-area                                              
040000     Perform S12-SKRIV-WDMR1G                                             
040100     .                                                                    
040200     eject                                                                
040300 CA-FIRST-NEW-RCD Section.                                                
040400                                                                          
040500     Move command     To ut-job-area                                      
040600     Perform S12-SKRIV-WDMR1G                                             
040700     Move rub-job     To ut-job-area                                      
040800     Perform S12-SKRIV-WDMR1G                                             
040900     Move Space       To ut-job-area                                      
041000     Perform S12-SKRIV-WDMR1G                                             
041100     Move rub-catalog To  ut-job-area                                     
041200     Perform S12-SKRIV-WDMR1G                                             
041300     Perform CAA-KATALOGORD                                               
041400     Perform S12-SKRIV-WDMR1G                                             
041500     If job-program = Space                                               
041600       Continue                                                           
041700     Else                                                                 
041800       Move rub-contains To ut-job-area                                   
041900       Perform S12-SKRIV-WDMR1G                                           
042000       Move ja To skrivit-contains                                        
042100       Move Space        To contains-komma                                
042200       Move job-program  To contains-mbr                                  
042300       Move contains-post To ut-job-area                                  
042400       Perform S12-SKRIV-WDMR1G                                           
042500     End-If                                                               
042600     .                                                                    
042700     eject                                                                
042800 CAA-KATALOGORD Section.                                                  
042900                                                                          
043000     Move 1     To pekare                                                 
043100     Move Space To cat-name-area                                          
043200     String fnutt     Delimited By Size                                   
043300            rub-job   Delimited By Space                                  
043400            fnutt     Delimited By Size                                   
043500       Into cat-name-area                                                 
043600       With Pointer pekare                                                
043700     Move cat-names To ut-job-area                                        
043800     .                                                                    
043900     eject                                                                
044000 CB-NEXT-NEW-RCD Section.                                                 
044100                                                                          
044200     If job-program = Space                                               
044300       Continue                                                           
044400     Else                                                                 
044500       If skrivit-contains = nej                                          
044600         Move rub-contains To ut-job-area                                 
044700         Perform S12-SKRIV-WDMR1G                                         
044800         Move ja To skrivit-contains                                      
044900         Move Space To contains-komma                                     
045000       Else                                                               
045100         Move ',' To contains-komma                                       
045200       End-If                                                             
045300       Move job-program To contains-mbr                                   
045400       Move contains-post To ut-job-area                                  
045500       Perform S12-SKRIV-WDMR1G                                           
045600     End-If                                                               
045700     .                                                                    
045800     eject                                                                
045900 CC-DELETE-RCD Section.                                                   
046000                                                                          
046100     Move cmd-remove      To ut-job-area                                  
046200     Perform S12-SKRIV-WDMR1G                                             
046500     .                                                                    
046600     eject                                                                
046700 D-SKAPA-PGM  Section.                                                    
046800                                                                          
046900     Move nej To end-punkt                                                
047000     Perform S03-LAES-WDMR13                                              
047100     Perform Until end-of-rdmr13                                          
047200       If spar-pgm  Not = pgm-program                                     
047300         Move nej To skrivit-contains                                     
047400         If end-punkt = nej                                               
047500           Move ja To end-punkt                                           
047600         Else                                                             
047700           Move '.' To ut-pgm-area                                        
047800           Perform S13-SKRIV-WDMR1H                                       
047900         End-if                                                           
048000         Move pgm-program   To cmd-member spar-pgm rem-member             
048100         If pgm-styr Not = 'D'                                            
048200           Perform DA-FIRST-NEW-RCD                                       
048300         Else                                                             
048400           Perform DC-DELETE-RCD                                          
048500         End-If                                                           
048600       Else                                                               
048700         Perform DB-NEXT-NEW-RCD                                          
048800       End-If                                                             
048900                                                                          
049000       Perform S03-LAES-WDMR13                                            
049100     End-Perform                                                          
049200     Move '.' To ut-pgm-area                                              
049300     Perform S13-SKRIV-WDMR1H                                             
049400     .                                                                    
049500     eject                                                                
049600 DA-FIRST-NEW-RCD Section.                                                
049700                                                                          
049800     Move command     To ut-pgm-area                                      
049900     Perform S13-SKRIV-WDMR1H                                             
050000     Move rub-job     To ut-pgm-area                                      
050100     Perform S13-SKRIV-WDMR1H                                             
050200     Move Space       To ut-pgm-area                                      
050300     Perform S13-SKRIV-WDMR1H                                             
050400     Move rub-catalog To  ut-pgm-area                                     
050500     Perform S13-SKRIV-WDMR1H                                             
050600     Perform DAA-KATALOGORD                                               
050700     Perform S13-SKRIV-WDMR1H                                             
050800                                                                          
052200     If pgm-psb Not = Space                                               
052300       If skrivit-contains = nej                                          
052400         Move rub-contains To ut-pgm-area                                 
052500         Perform S13-SKRIV-WDMR1H                                         
052600         Move ja To skrivit-contains                                      
052700         Move Space to contains-komma                                     
052800       Else                                                               
052900         Move ','     To  contains-komma                                  
053000       End-If                                                             
053100       Move pgm-psb To contains-mbr                                       
053200       Move contains-post To ut-pgm-area                                  
053300       Perform S13-SKRIV-WDMR1H                                           
053400     End-If                                                               
053500                                                                          
053600     If pgm-dsnamn Not = Space                                            
053900       If skrivit-contains = nej                                          
054000         Move rub-contains To ut-pgm-area                                 
054100         Perform S13-SKRIV-WDMR1H                                         
054200         Move ja To skrivit-contains                                      
054300         Move Space To contains-komma                                     
054400       Else                                                               
054500         Move ',' To contains-komma                                       
054600       End-If                                                             
054700       Move pgm-dsnamn   To contains-mbr                                  
054800       Move contains-post To ut-pgm-area                                  
054900       Perform S13-SKRIV-WDMR1H                                           
055000     End-If                                                               
055001                                                                          
055010     If pgm-laddmodul Not = Space                                         
055040       If skrivit-contains = nej                                          
055050         Move rub-contains To ut-pgm-area                                 
055060         Perform S13-SKRIV-WDMR1H                                         
055070         Move ja To skrivit-contains                                      
055080         Move Space To contains-komma                                     
055090       Else                                                               
055091         Move ',' To contains-komma                                       
055092       End-If                                                             
055093       Move pgm-laddmodul To contains-mbr                                 
055094       Move contains-post To ut-pgm-area                                  
055095       Perform S13-SKRIV-WDMR1H                                           
055096     End-If                                                               
055100     .                                                                    
055200     eject                                                                
055300 DAA-KATALOGORD Section.                                                  
055400                                                                          
055500     Move 1     To pekare                                                 
055600     Move Space To cat-name-area                                          
055700     String fnutt     Delimited By Size                                   
055800            rub-job   Delimited By Space                                  
055900            fnutt     Delimited By Size                                   
056000       Into cat-name-area                                                 
056100       With Pointer pekare                                                
056200     If pgm-pgmtyp Not = Space                                            
056210       If pgm-pgmtyp(1:3) = 'IMS'                                         
056211         String ', '     Delimited By Size                                
056212                fnutt    Delimited By Size                                
056213                'IMS'    Delimited By Size                                
056214                fnutt    Delimited By Size                                
056215           Into cat-name-area                                             
056216           With Pointer pekare                                            
056217         If pgm-pgmtyp = 'IMS-UTL'                                        
056218           Move 'STDPGM' To w-cat-word                                    
056219         Else                                                             
056220           Move pgm-pgmtyp(5:4) To w-cat-word                             
056221         End-If                                                           
056223         String ', '     Delimited By Size                                
056224                fnutt    Delimited By Size                                
056225                w-cat-word Delimited By Space                             
056226                fnutt    Delimited By Size                                
056227           Into cat-name-area                                             
056228           With Pointer pekare                                            
056230       Else                                                               
056300         String ', '     Delimited By Size                                
056400                fnutt    Delimited By Size                                
056500                pgm-pgmtyp Delimited By Space                             
056600                fnutt    Delimited By Size                                
056700           Into cat-name-area                                             
056800           With Pointer pekare                                            
056810       End-If                                                             
056900       If pgm-pgmtyp = 'V16459'                                           
057000         String ', '       Delimited By Size                              
057100                fnutt      Delimited By Size                              
057200                'STDPGM'   Delimited By Space                             
057300                fnutt      Delimited By Size                              
057400           Into cat-name-area                                             
057500           With Pointer pekare                                            
057600       End-If                                                             
057700     End-If                                                               
057800     Move cat-names To ut-pgm-area                                        
057900     .                                                                    
058000     eject                                                                
058100 DB-NEXT-NEW-RCD Section.                                                 
058200                                                                          
058300     If pgm-dsnamn  = Space                                               
058400       Continue                                                           
058500     Else                                                                 
058600       If skrivit-contains = nej                                          
058700         Move rub-contains To ut-pgm-area                                 
058800         Perform S13-SKRIV-WDMR1H                                         
058900         Move ja To skrivit-contains                                      
059000         Move Space To contains-komma                                     
059100       Else                                                               
059200         Move ',' To contains-komma                                       
059300       End-If                                                             
059400       Move pgm-dsnamn  To contains-mbr                                   
059500       Move contains-post To ut-pgm-area                                  
059600       Perform S13-SKRIV-WDMR1H                                           
059700     End-If                                                               
059800     .                                                                    
059900     eject                                                                
060000 DC-DELETE-RCD Section.                                                   
060100                                                                          
060200     Move cmd-remove      To ut-pgm-area                                  
060300     Perform S13-SKRIV-WDMR1H                                             
060600     .                                                                    
060700     eject                                                                
060800 E-SKAPA-DSN  Section.                                                    
060900                                                                          
061000     Move nej To end-punkt                                                
061100     Perform S04-LAES-WDMR14                                              
061200     Perform Until end-of-rdmr14                                          
061300       If spar-dsn  Not = dsn-dsnamn                                      
061400         Move nej To skrivit-contains                                     
061500         If end-punkt = nej                                               
061600           Move ja To end-punkt                                           
061700         Else                                                             
061800           Move '.' To ut-dsn-area                                        
061900           Perform S14-SKRIV-WDMR1I                                       
062000         End-if                                                           
062100         Move dsn-dsnamn    To cmd-member spar-dsn rem-member             
062200         If dsn-styr Not = 'D'                                            
062300           Perform EA-FIRST-NEW-RCD                                       
062400         Else                                                             
062500           Perform EC-DELETE-RCD                                          
062600         End-If                                                           
062700       Else                                                               
062800         Perform EB-NEXT-NEW-RCD                                          
062900       End-If                                                             
063000                                                                          
063100       Perform S04-LAES-WDMR14                                            
063200     End-Perform                                                          
063300     Move '.' To ut-dsn-area                                              
063400     Perform S14-SKRIV-WDMR1I                                             
063500     .                                                                    
063600     eject                                                                
063700 EA-FIRST-NEW-RCD Section.                                                
063800                                                                          
063900     Move command     To ut-dsn-area                                      
064000     Perform S14-SKRIV-WDMR1I                                             
064100     Move rub-dsn     To ut-dsn-area                                      
064200     Perform S14-SKRIV-WDMR1I                                             
064300     Move Space       To ut-dsn-area                                      
064400     Perform S14-SKRIV-WDMR1I                                             
064500     Move rub-catalog To  ut-dsn-area                                     
064600     Perform S14-SKRIV-WDMR1I                                             
064700     Perform EAA-KATALOGORD                                               
064800     Perform S14-SKRIV-WDMR1I                                             
064900     If dsn-ddnamn  = Space                                               
065000       Continue                                                           
065100     Else                                                                 
065200       Move rub-contains To ut-dsn-area                                   
065300       Perform S14-SKRIV-WDMR1I                                           
065400       Move ja To skrivit-contains                                        
065500       Move Space        To contains-komma                                
065600       Move dsn-ddnamn   To contains-mbr                                  
065700       Move contains-post To ut-dsn-area                                  
065800       Perform S14-SKRIV-WDMR1I                                           
065900     End-If                                                               
066000     .                                                                    
066100     eject                                                                
066200 EAA-KATALOGORD Section.                                                  
066300                                                                          
066400     Move 1     To pekare                                                 
066500     Move Space To cat-name-area                                          
066600     String fnutt     Delimited By Size                                   
066700            rub-dsn   Delimited By Space                                  
066800            fnutt     Delimited By Size                                   
066900       Into cat-name-area                                                 
067000       With Pointer pekare                                                
067100     Move cat-names To ut-dsn-area                                        
067200     .                                                                    
067300     eject                                                                
067400 EB-NEXT-NEW-RCD Section.                                                 
067500                                                                          
067600     If dsn-ddnamn  = Space                                               
067700       Continue                                                           
067800     Else                                                                 
067900       If skrivit-contains = nej                                          
068000         Move rub-contains To ut-dsn-area                                 
068100         Perform S14-SKRIV-WDMR1I                                         
068200         Move ja To skrivit-contains                                      
068300         Move Space To contains-komma                                     
068400       Else                                                               
068500         Move ',' To contains-komma                                       
068600       End-If                                                             
068700       Move dsn-ddnamn  To contains-mbr                                   
068800       Move contains-post To ut-dsn-area                                  
068900       Perform S14-SKRIV-WDMR1I                                           
069000     End-If                                                               
069100     .                                                                    
069200     eject                                                                
069300 EC-DELETE-RCD Section.                                                   
069400                                                                          
069500     Move cmd-remove      To ut-dsn-area                                  
069600     Perform S14-SKRIV-WDMR1I                                             
069900     .                                                                    
070000     eject                                                                
070100 Z-FINIT Section.                                                         
070200     Close                                                                
070300       WDMR11 WDMR12 WDMR13 WDMR14 WDMR1F WDMR1G WDMR1H WDMR1I            
070400     skip2                                                                
070500     Move 'S' To postsum-opkod                                            
070600     Call POSTSUM Using postsum-parm                                      
070700     .                                                                    
070800     eject                                                                
070900 S01-LAES-WDMR11  Section.                                                
071000     skip2                                                                
071100     Read WDMR11 Into in-proc-area                                        
071200     At End                                                               
071300        Set end-of-rdmr11 To True                                         
071400                                                                          
071500     Not At End                                                           
071600        Move 'WDMR11'    To postsum-fdnamn                                
071700        Move 'WDMR18D1'  To postsum-ddnamn2                               
071800        Call POSTSUM Using postsum-parm                                   
071900     End-Read                                                             
072000     .                                                                    
072100     eject                                                                
072200 S02-LAES-WDMR12  Section.                                                
072300     skip2                                                                
072400     Read WDMR12 Into in-job-area                                         
072500     At End                                                               
072600        Set end-of-rdmr12 To True                                         
072700                                                                          
072800     Not At End                                                           
072900        Move 'WDMR12'   To postsum-fdnamn                                 
073000        Move 'WDMR18D2' To postsum-ddnamn2                                
073100        Call POSTSUM Using postsum-parm                                   
073200     End-Read                                                             
073300     .                                                                    
073400     eject                                                                
073500 S03-LAES-WDMR13  Section.                                                
073600     skip2                                                                
073700     Read WDMR13 Into in-pgm-area                                         
073800     At End                                                               
073900        Set end-of-rdmr13 To True                                         
074000                                                                          
074100     Not At End                                                           
074200        Move 'WDMR13'   To postsum-fdnamn                                 
074300        Move 'WDMR18D3' To postsum-ddnamn2                                
074400        Call POSTSUM Using postsum-parm                                   
074500     End-Read                                                             
074600     .                                                                    
074700     eject                                                                
074800 S04-LAES-WDMR14  Section.                                                
074900     skip2                                                                
075000     Read WDMR14 Into in-dsn-area                                         
075100     At End                                                               
075200        Set end-of-rdmr14 To True                                         
075300                                                                          
075400     Not At End                                                           
075500        Move 'WDMR14'   To postsum-fdnamn                                 
075600        Move 'WDMR18D4' To postsum-ddnamn2                                
075700        Call POSTSUM Using postsum-parm                                   
075800     End-Read                                                             
075900     .                                                                    
076000     eject                                                                
076100 S11-SKRIV-WDMR1F Section.                                                
076200     skip2                                                                
076300     Write proc-post From ut-proc-area                                    
076400                                                                          
076500     Move Space      To postsum-transtyp                                  
076600     Move 'WDMR1F'   To postsum-fdnamn                                    
076700     Move 'WDMR18D5' To postsum-ddnamn2                                   
076800     Call POSTSUM Using postsum-parm                                      
076900     .                                                                    
077000     eject                                                                
077100 S12-SKRIV-WDMR1G Section.                                                
077200     skip2                                                                
077300     Write job-post From ut-job-area                                      
077400                                                                          
077500     Move Space      To postsum-transtyp                                  
077600     Move 'WDMR1G'   To postsum-fdnamn                                    
077700     Move 'WDMR18D6' To postsum-ddnamn2                                   
077800     Call POSTSUM Using postsum-parm                                      
077900     .                                                                    
078000     eject                                                                
078100 S13-SKRIV-WDMR1H Section.                                                
078200     skip2                                                                
078300     Write pgm-post From ut-pgm-area                                      
078400                                                                          
078500     Move Space      To postsum-transtyp                                  
078600     Move 'WDMR1H'   To postsum-fdnamn                                    
078700     Move 'WDMR18D7' To postsum-ddnamn2                                   
078800     Call POSTSUM Using postsum-parm                                      
078900     .                                                                    
079000     eject                                                                
079100 S14-SKRIV-WDMR1I Section.                                                
079200     skip2                                                                
079300     Write dsn-post From ut-dsn-area                                      
079400                                                                          
079500     Move Space      To postsum-transtyp                                  
079600     Move 'WDMR1I'   To postsum-fdnamn                                    
079700     Move 'WDMR18D8' To postsum-ddnamn2                                   
079800     Call POSTSUM Using postsum-parm                                      
079900     .                                                                    
080000     eject                                                                
080100 S99-ABEND Section.                                                       
080200                                                                          
080300     skip2                                                                
080400     Move 'S' To postsum-opkod                                            
080500     Call POSTSUM Using postsum-parm                                      
080600     Call ABEND Using rkod-abend-utan-dump                                
080700     .                                                                    
