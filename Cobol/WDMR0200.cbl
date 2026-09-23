000100 Id Division.                                                             
000200     skip2                                                                
000300 Program-Id.     wdmr0200.                                                
000400*AUTHOR.         ODD OLSEN.                                               
000500*DATE-WRITTEN.   91/12/17.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        Suga ut information till Data Manager från proclib               
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
002500*          --- PROCLIB MEDLEMMAR                                          
002600     Select wdmr01                     Assign To wdmr02D1.                
002700     skip2                                                                
002800*          --- Information för vidare bearbetning                         
002900     Select wdmr02                     Assign To wdmr02D2.                
003000     skip2                                                                
003100*          --- Information för vidare bearbetning                         
003200     Select wdmr03                     Assign To wdmr02D3.                
003300     eject                                                                
003400 Data Division.                                                           
003500     skip3                                                                
003600 File Section.                                                            
003700     skip3                                                                
003800 Fd  wdmr01                                                               
003900     Recording       F                                                    
004000     Block Contains  0.                                                   
004100     skip2                                                                
004200 01  Filler                      Pic x(132).                              
004300     skip3                                                                
004400 Fd  wdmr02                                                               
004500     Recording       F                                                    
004600     Block Contains  0.                                                   
004700     skip2                                                                
004800 01  ut-post                     Pic x(120).                              
004900     skip3                                                                
005000 Fd  wdmr03                                                               
005100     Recording       F                                                    
005200     Block Contains  0.                                                   
005300     skip2                                                                
005400 01  sysout-post                 Pic x(12).                               
005500     eject                                                                
005600 Working-Storage Section.                                                 
005601                                                                          
005610*    -- CHECKED BY WY2000                                                 
005700     skip2                                                                
005800 77  idpgm                       Pic X(8)    Value 'wdmr0200'.            
005900 77  ja                          Pic X       Value 'J'.                   
006000 77  nej                         Pic X       Value 'N'.                   
006100 77  fnutt                       Pic X       Value ''''.                  
006200 77  expecting-dd                Pic X       Value 'N'.                   
006300 77  expecting-dli-parm          Pic X       Value 'N'.                   
006400 77  expecting-concat-dd         Pic X       Value 'N'.                   
006500 77  expecting-sort-kort         Pic X       Value 'N'.                   
006600 77  expecting-iebgener-kort     Pic X       Value 'N'.                   
006700 77  expecting-symb-parm         Pic X       Value 'N'.                   
006800 77  w-str                       Pic X(200)  Value Space.                 
006900 77  w-str2                      Pic X(200)  Value Space.                 
007000 77  w-str3                      Pic X(200)  Value Space.                 
007100 77  w-ddnamn                    Pic X(200)  Value Space.                 
007200 77  symb-var-str                Pic X(200)  Value Space.                 
007210 77  w-member                    Pic X(8)    Value Space.                 
007300 77  spar-proc-mbr               Pic X(8)    Value Space.                 
007400 77  dli-program                 Pic X(8)  Value 'DFSRRC00'.              
007600 77  fsu-program                 Pic X(8)  Value 'IMSFSU'.                
007700 77  pekare                      Pic 9(4)  Comp Value Zero.               
007800 77  next-word                   Pic 9(4)  Comp Value Zero.               
007900 77  ix                          Pic 9(4)  Comp Value Zero.               
008000 77  w-len                       Pic 9(4)  Comp Value Zero.               
008100 77  symb-ix                     Pic 9(4)  Comp Value Zero.               
008200 77  symb-start                  Pic 9(4)  Comp Value Zero.               
008300 77  symb-end                    Pic 9(4)  Comp Value Zero.               
008400 77  p-start                     Pic 9(4)  Comp Value Zero.               
008500 77  p-len                       Pic 9(4)  Comp Value Zero.               
008600                                                                          
008700 77  proc-sw                     Pic X       Value 'N'.                   
008800     88  expecting-proc                      Value 'N'.                   
008900     88  proc-found                          Value 'J'.                   
009000                                                                          
009100 77  post-kontroll-sw            Pic X       Value 'N'.                   
009200     88  post-fel                            Value 'N'.                   
009300     88  post-ok                             Value 'J'.                   
009400                                                                          
009500 77  infil-eof-sw                Pic X       Value 'N'.                   
009600     88  end-of-infil                        Value 'J'.                   
009700                                                                          
009701 77  w-symb-parm                 Pic x(8).                                
009710 77  max-ix                      Pic S9(4)   Comp Value +100.             
009800 01  symb-parms-grp.                                                      
009900     03 symb-parms occurs 100.                                            
010000       05 symb-parm              Pic x(8).                                
010100       05 symb-value             Pic x(50).                               
010200       05 symb-len               Pic 9(4) Comp.                           
010300     eject                                                                
010410 01  dagens-datum                Pic 9(6)    Value Zero.                  
010500 01  Filler Redefines dagens-datum.                                       
010600     03  dagens-datum-aar        Pic 9(2).                                
010700     03  dagens-datum-maanad     Pic 9(2).                                
010800     03  dagens-datum-dag        Pic 9(2).                                
010900     eject                                                                
011000 01  dynamiska-subprogram.                                                
011100*                                                                         
011200     03  abend                   Pic X(8)    Value 'ABEND'.               
011300     03  wordind                 Pic X(8)    Value 'wdmr0210'.            
011400     03  parmfind                Pic X(8)    Value 'wdmr0220'.            
011500     skip2                                                                
011600*    --- PARAMETRAR TILL ABEND                                            
011700                                                                          
011800 77  rkod-abend-utan-dump        Pic S9(4)   Comp Value +16.              
011900 77  rkod-abend-med-dump         Pic S9(4)   Comp Value +1000.            
012000     skip2                                                                
012100 01  feltext.                                                             
012200     03  Filler                  Pic X(8)    Value 'FELTEXT'.             
012300     03  feltext-str             Pic X(72)   Value Space.                 
012400     eject                                                                
012500 01  wordind-parms.                                                       
012600     03 word-number         Pic 9(4) Comp.                                
012700     03 word-type           Pic 9(4) Comp.                                
012800     03 start-pos           Pic 9(4) Comp.                                
012900     03 end-pos             Pic 9(4) Comp.                                
013000     03 word-sub-del        Pic x.                                        
013100     03 word-string         Pic x(200).                                   
013200     03 word-pos            Pic 9(4) Comp.                                
013300     03 word-length         Pic 9(4) Comp.                                
013400     03 leading-space       Pic 9(4) Comp.                                
013500 01  parmfind-parms.                                                      
013600     03 parm-del            Pic x.                                        
013700     03 parm-str-del        Pic x.                                        
013800     03 parm-string         Pic x(200).                                   
013900     03 parm-pos            Pic 9(4).                                     
014000     03 parm-length         Pic 9(4) Comp.                                
014100     eject                                                                
014200 01  in-area-start               Pic X(24)   Value                        
014300                                 'IN-AREA-START  '.                       
014400     skip2                                                                
014500                                                                          
014600 01  in-area                Pic x(150).                                   
015010 01  filler redefines in-area.                                            
015030     03                     pic x(63).                                    
015031     03 in-dsname-ord       pic x(14).                                    
015050     03 in-dsn-mbr          pic x(55).                                    
015060 01  filler redefines in-area.                                            
015070     03                     pic x(1).                                     
015080     03 in-rec-ord          pic x(3).                                     
015081     03                     pic x(26).                                    
015090     03 in-jcl-rad.                                                       
015091       05 jcl-kommentar     pic x(3).                                     
015092       05                   pic x(77).                                    
015100     eject                                                                
015200 01  ut-area-start               Pic X(24)   Value                        
015300                                 'UT-AREA-START  '.                       
015400     skip2                                                                
015500                                                                          
015600 01  ut-area.                                                             
015700     03 ut-procedur              Pic x(8).                                
015800     03 ut-steg                  Pic x(8).                                
015900     03 ut-program               Pic x(22).                               
016000     03 ut-pgmtyp                Pic x(8).                                
016100     03 ut-psb                   Pic x(8).                                
016200     03 ut-ddnamn                Pic x(20).                               
016300     03 ut-dsnamn                Pic x(41).                               
016400     eject                                                                
016500 Linkage Section.                                                         
016600                                                                          
016700 01  parm.                                                                
016800     03  parm-laengd             Pic S9(4)   Comp Sync.                   
016900     03  parm-vaerde             Pic X(2).                                
017000     eject                                                                
017100 Procedure Division Using parm.                                           
017200                                                                          
017300 STYR Section.                                                            
017400                                                                          
017500     Perform A-INIT                                                       
017600                                                                          
017700     Perform S01-LAES-INFIL                                               
017800     Perform Until end-of-infil                                           
017900       Set post-ok To True                                                
018000       Evaluate True                                                      
018100         When in-dsname-ord = 'Data Set Name:'                            
018110           move zero to tally                                             
018120           inspect in-dsn-mbr tallying tally for                          
018130             characters before initial '('                                
018131           add 2 to tally                                                 
018140           unstring in-dsn-mbr delimited by ')'                           
018150             into w-member                                                
018160             with pointer tally                                           
018200           If w-member Not = spar-proc-mbr                                
018300             Move w-member To spar-proc-mbr                               
018400             Set expecting-proc To True                                   
018500           End-If                                                         
018600         When in-rec-ord = 'Rec' or 'REC'                                 
018610           If jcl-kommentar = '//*'                                       
018700             Continue                                                     
018800           Else                                                           
018900             Perform B-LETA-KORTTYP                                       
018910           End-if                                                         
019000       End-Evaluate                                                       
019100       Perform S01-LAES-INFIL                                             
019200     End-Perform                                                          
019201                                                                          
019210     If expecting-dd = ja                                                 
019211*      -- inga filer i sista proceduren - skriv en post för steget        
019212       Perform S11-SKRIV-UTFIL                                            
019220     End-if                                                               
019300                                                                          
019400     Perform Z-FINIT                                                      
019500                                                                          
019600     Move Zero To return-code                                             
019700     Goback                                                               
019800     .                                                                    
019900     eject                                                                
020000 A-INIT Section.                                                          
020100                                                                          
020200     Open Input  wdmr01                                                   
020300     Open Output wdmr02 wdmr03                                            
020400                                                                          
020500     Accept dagens-datum  From Date                                       
020600                                                                          
020700     Move fnutt To word-sub-del                                           
020800     .                                                                    
020900     eject                                                                
021000 B-LETA-KORTTYP Section.                                                  
021100     skip2                                                                
021200     Move 1 to start-pos                                                  
021300     Move 80 to end-pos                                                   
021400     Move 1  to word-type                                                 
021500     Move 2  to word-number                                               
021600     Move in-jcl-rad to word-string                                       
021700     Call WORDIND Using wordind-parms                                     
021800     Evaluate True                                                        
021900       When word-pos = 0                                                  
022000         Continue                                                         
022100       When word-string(word-pos:word-length) = 'PROC'                    
022200         Set proc-found To True                                           
022300         Move nej to expecting-dli-parm                                   
022400         Perform BA-BEHANDLA-PROC                                         
022500       When word-string(word-pos:word-length) = 'EXEC'                    
022600         Move nej to expecting-dli-parm                                   
022700         If proc-found                                                    
022800           Perform BB-BEHANDLA-EXEC                                       
022810           Move ja to expecting-dd                                        
022900         Else                                                             
023000         Display '--EXEC outside PROC in procedure ' spar-proc-mbr        
023100         End-If                                                           
023200       When word-string(word-pos:word-length) = 'DD'                      
023300         Move nej to expecting-dli-parm                                   
023400         If proc-found                                                    
023500           Perform BC-BEHANDLA-DDNAMN                                     
023600         Else                                                             
023700           Continue                                                       
023800         End-If                                                           
023900       When Other                                                         
024000         Perform BZ-ANNAT                                                 
024100     End-Evaluate                                                         
024200     .                                                                    
024300     eject                                                                
024400 BA-BEHANDLA-PROC Section.                                                
024500     skip2                                                                
024600     If expecting-dd = ja                                                 
024610*      -- inga filer i föregående proc - skriv en post för steget         
024700       Perform S11-SKRIV-UTFIL                                            
024800       Move nej to expecting-dd                                           
024900     End-If                                                               
025000                                                                          
025100     Move space to ut-area                                                
025200                                                                          
025300     Move 1  to word-number                                               
025400     Call WORDIND Using wordind-parms                                     
025500     Move word-string(word-pos + 2:word-length - 1) to ut-procedur        
025600     If ut-procedur Not = spar-proc-mbr                                   
025700       Display '--Name Error'                                             
025800       Display '  Member name = ' spar-proc-mbr                           
025900       Display '  Proc name   = ' ut-procedur                             
026000       Move spar-proc-mbr To ut-procedur                                  
026100     End-If                                                               
026200                                                                          
026300     Move ja to expecting-symb-parm                                       
026400                                                                          
026500     Initialize symb-parms-grp                                            
026600     Move Zero  To symb-ix                                                
026610     Perform S53-SET-ENV-PARM                                             
026700     Move 3     To word-number                                            
026800     Perform S52-HITTA-SYMB-PARM                                          
026900     .                                                                    
027000     eject                                                                
027100 BB-BEHANDLA-EXEC Section.                                                
027200     skip2                                                                
027300     If expecting-dd = ja                                                 
027310*      -- inga filer i föregående steg - skriv en post för ändå           
027400       Perform S11-SKRIV-UTFIL                                            
027500       Move nej to expecting-dd                                           
027600     End-If                                                               
027700                                                                          
027800     Move nej to expecting-sort-kort expecting-iebgener-kort              
027900     Move nej to expecting-symb-parm                                      
028000                                                                          
028100     Move Space To ut-ddnamn ut-program ut-pgmtyp ut-psb ut-dsnamn        
028200                                                                          
028300* Sätt eventuella symboliska variabler                                    
028400     Move word-string To symb-var-str                                     
028500     Perform S60-SET-SYMB-VARS                                            
028600     Move symb-var-str To word-string                                     
028700                                                                          
028800     Move 1  to word-number                                               
028900     Call WORDIND Using wordind-parms                                     
029000                                                                          
029100* Hitta stegnamn                                                          
029200     Move word-string(word-pos + 2:word-length - 1) to ut-steg            
029300                                                                          
029400* Hitta programnamn                                                       
029500     Move 3   to word-number                                              
029600     Call WORDIND Using wordind-parms                                     
029700     If word-pos > 0                                                      
029800       Move word-string(word-pos:word-length) to w-str2                   
029900       Unstring w-str2                                                    
030000           Delimited By ',' Or Space                                      
030100           Into w-str Count In pekare                                     
030200       If w-str(1:4) = 'PGM='                                             
030300         Move w-str(5:) To ut-program                                     
030400       Else                                                               
030500         Set post-fel To True                                             
030600         Display '--' w-str(1:pekare) ' following an EXEC-card in'        
030700         Display '  procedure ' spar-proc-mbr ' is not a program'         
030800       End-If                                                             
030900     End-If                                                               
031000                                                                          
031100* Om DLI-program                                                          
031200     Evaluate True                                                        
031300       When ut-program = dli-program                                      
031500         Add 2 to pekare                                                  
031600         Move ja to expecting-dli-parm                                    
031700         If w-str2(pekare:) = Space                                       
031800           Continue                                                       
031900         Else                                                             
032000           Move 3 To word-number                                          
032100           Perform S51-HITTA-PSB                                          
032200         End-If                                                           
032300       When ut-program(1:3) = 'DFS'                                       
032400         Move 'IMS-UTL' To ut-pgmtyp                                      
032500       When ut-program = 'IMSFSU'                                         
032600         Move 'IMS-FSU' To ut-pgmtyp                                      
032700     End-Evaluate                                                         
032800                                                                          
032900* Om V16459-program                                                       
033000     If ut-program = 'V16459' or ut-program = 'V16459RS'                  
033100       Add 2 to pekare                                                    
033200       Move w-str2(pekare:) To w-str3                                     
033300       If w-str3(1:5) = 'PARM='                                           
033400         Move ut-program To ut-pgmtyp                                     
033500         Move w-str3(6:) To w-str3                                        
033600         Move Space To ut-program                                         
033700         Unstring w-str3 Delimited By '/'                                 
033800             Into ut-program                                              
033900         If ut-program(1:1) = fnutt                                       
034000           Move ut-program(2:) To ut-program                              
034100         End-If                                                           
034200       End-If                                                             
034300     End-If                                                               
034400     .                                                                    
034500     eject                                                                
034600 BC-BEHANDLA-DDNAMN Section.                                              
034700     skip2                                                                
034800     Move 1  to word-number                                               
034900     Call WORDIND Using wordind-parms                                     
035000     Move word-string(word-pos:word-length) To w-str                      
035100     Evaluate True                                                        
035210       When w-str(3:6) = ut-program(1:6) Or                               
035220            w-str(3:1) = 'W' And w-str(9:1) = 'D'                         
035300         Move w-str(3:8) To ut-ddnamn                                     
035400         Move nej to expecting-dd                                         
035500         Perform S55-HITTA-DSNAMN                                         
035600         Perform S11-SKRIV-UTFIL                                          
035700         Move ja to expecting-concat-dd                                   
035800       When word-length = 2                                               
035900         If expecting-concat-dd = ja                                      
036000           Perform S55-HITTA-DSNAMN                                       
036100           Perform S11-SKRIV-UTFIL                                        
036200         End-If                                                           
036300       When Other                                                         
036400         Move w-str To w-ddnamn                                           
036500* Hämta texten efter DD                                                   
036600         Move 3 To word-number                                            
036700         Call WORDIND Using wordind-parms                                 
036800         If word-pos > 0                                                  
036900           Move word-string(word-pos:word-length) To symb-var-str         
037000           Perform S60-SET-SYMB-VARS                                      
037100           Move symb-var-str To w-str                                     
037200* Kolla om dsnamn:et börjar med W, men ta inte med FADUMP                 
037300           If w-ddnamn(3:8) not = 'IDIHIST ' And                          
037310            ( w-str(1:5) = 'DSN=W'   Or                                   
037400              w-str(1:6) = 'DSN=&W'  Or                                   
037500              w-str(1:7) = 'DSN=&&W'     )                                
037600             Move Space to ut-ddnamn                                      
037700             String ut-program    Delimited By Space                      
037800                    '-'           Delimited By Size                       
037900                    w-ddnamn(3:8) Delimited By Size                       
038000               Into ut-ddnamn                                             
038100             Move nej to expecting-dd                                     
038200             Perform S55-HITTA-DSNAMN                                     
038300             Perform S11-SKRIV-UTFIL                                      
038400             Move ja to expecting-concat-dd                               
038500* Om det inte börjar med W så skall inte DD-kortet med                    
038600           Else                                                           
038700             Move nej to expecting-concat-dd                              
038800           End-If                                                         
038900         End-If                                                           
039000     End-Evaluate                                                         
039100     .                                                                    
039200     eject                                                                
039300 BZ-ANNAT Section.                                                        
039400     skip2                                                                
039500     If expecting-dli-parm = ja                                           
039600       Move 2 To word-number                                              
039700       Perform S51-HITTA-PSB                                              
039800     End-If                                                               
039900     If expecting-symb-parm = ja                                          
040000       Move 2 to word-number                                              
040100       Perform S52-HITTA-SYMB-PARM                                        
040200     End-If                                                               
040300     .                                                                    
040400     eject                                                                
040500 Z-FINIT Section.                                                         
040600     skip2                                                                
040700     Close wdmr01 wdmr02 wdmr03                                           
040800     .                                                                    
040900     eject                                                                
041000 S01-LAES-INFIL  Section.                                                 
041100     skip2                                                                
041200     Read wdmr01 Into in-area                                             
041300       At End                                                             
041400          Set end-of-infil To True                                        
041500       End-Read                                                           
041600     .                                                                    
041700     skip3                                                                
041800 S11-SKRIV-UTFIL Section.                                                 
041900     skip2                                                                
042000     If post-ok                                                           
042100       Write ut-post From ut-area                                         
042200     End-If                                                               
042300     .                                                                    
042400     eject                                                                
042500 S12-SKRIV-UTFIL-02 Section.                                              
042600     skip2                                                                
042700     Write sysout-post From ut-ddnamn                                     
042800     .                                                                    
042900     eject                                                                
043000 S51-HITTA-PSB Section.                                                   
043100     skip2                                                                
043200* Sätt symboliska variabler                                               
043300     Move word-string To symb-var-str                                     
043400     Perform S60-SET-SYMB-VARS                                            
043500     Move symb-var-str To word-string                                     
043600                                                                          
043700     Call WORDIND Using wordind-parms                                     
043800     If word-pos > Zero                                                   
043900       Move word-string(word-pos:word-length) To w-str                    
044000       Move ','   To parm-del                                             
044100       Move fnutt To parm-str-del                                         
044200       Move w-str To parm-string                                          
044300       Call PARMFIND Using parmfind-parms                                 
044400       Perform Until parm-pos = Zero                                      
044500         Move parm-string(parm-pos:parm-length) To w-str                  
044600         If w-str(1:5) = 'PARM='                                          
044700           Move ut-program To ut-pgmtyp                                   
044800           Move w-str(8:)  To w-str                                       
044900           Unstring w-str Delimited By ','                                
045000              Into w-str2 ut-program ut-psb                               
045100           Inspect ut-program Replacing All fnutt By Space                
045200           Inspect ut-psb     Replacing All fnutt By Space                
045300* Sätt olika typer av IMS-program                                         
045400           Evaluate True                                                  
045500             When ut-program(1:3) = 'DFS'                                 
045600               Move 'IMS-UTL'  To ut-pgmtyp                               
045700             When w-str2 = 'BMP'                                          
045800               Move 'IMS-BMP'  To ut-pgmtyp                               
045900             When ut-pgmtyp = dli-program                                 
046000               Move 'IMS-DL1'  To ut-pgmtyp                               
046300           End-Evaluate                                                   
046400           Move nej To expecting-dli-parm                                 
046500           Move Zero To parm-pos                                          
046600         Else                                                             
046700           Move parm-string(parm-pos + parm-length + 1:)                  
046800             To parm-string                                               
046900           Call PARMFIND Using parmfind-parms                             
047000         End-If                                                           
047100       End-Perform                                                        
047200     End-If                                                               
047300                                                                          
047400     .                                                                    
047500     eject                                                                
047600 S52-HITTA-SYMB-PARM Section.                                             
047700     skip2                                                                
047800     Call WORDIND Using wordind-parms                                     
047900     If word-pos > 0                                                      
048000       Move word-string(word-pos:word-length) To w-str                    
048100                                                                          
048200       Move ','   To parm-del                                             
048300       Move fnutt To parm-str-del                                         
048400       Move w-str to parm-string                                          
048500                                                                          
048600       Call PARMFIND Using parmfind-parms                                 
048700                                                                          
048800       Perform Until parm-pos = 0 or symb-ix = max-ix                     
048900         If parm-length <= 0                                              
048901           Move Space To w-str                                            
048910         Else                                                             
049000           Move parm-string(parm-pos:parm-length) To w-str                
049010         End-if                                                           
049100         Unstring w-str Delimited By '='                                  
049200             Into w-symb-parm                                             
049300             Count In pekare                                              
049400                                                                          
049500         If pekare = 200                                                  
049600*   Det finns inget likamedtecken?! bryt loopen                           
049700           Move Zero to parm-pos                                          
049800           If w-str(1:1) Not Numeric                                      
049900             Display '--Invalid parm in proc ' spar-proc-mbr              
050000             Display '  ' w-str(1:50)                                     
050100           End-if                                                         
050200         Else                                                             
050210           Add 1 to symb-ix                                               
050220           Move w-symb-parm to symb-parm(symb-ix)                         
050300           Add 2 To pekare                                                
050400           Move w-str(pekare:) to w-str                                   
050500           If w-str(1:1) = fnutt                                          
050600             Move w-str(2:) To w-str                                      
050700             Unstring w-str Delimited By fnutt                            
050800                 Into symb-value(symb-ix)                                 
050900             Compute symb-len(symb-ix) =                                  
051000                        parm-length - pekare - 3 + 2                      
051100           Else                                                           
051200             Move w-str To symb-value(symb-ix)                            
051300             Compute symb-len(symb-ix) =                                  
051400                        parm-length - pekare - 1 + 2                      
051500           End-If                                                         
051600* Om en parameter har värdet DUMMY                                        
051700           If symb-value(symb-ix) = 'DUMMY'                               
051800             Display '--DUMMY-parm in procedure ' spar-proc-mbr           
051900           End-IF                                                         
052000* Fortsätt leta                                                           
052100           Move parm-string(parm-pos + parm-length + 1:)                  
052200             To parm-string                                               
052300                                                                          
052400           Call PARMFIND Using parmfind-parms                             
052500         End-If                                                           
052600       End-Perform                                                        
052700     End-If                                                               
052701     .                                                                    
052702     eject                                                                
052703 S53-SET-ENV-PARM    Section.                                             
052710                                                                          
052721     If symb-ix <= max-ix                                                 
052722       Add 1 to symb-ix                                                   
052730       Move 'INDRTE'  to symb-parm(symb-ix)                               
052740       Move 'W.QASE'  to symb-value(symb-ix)                              
052750       Move 6         to symb-len(symb-ix)                                
052751     End-if                                                               
052760                                                                          
052761     If symb-ix <= max-ix                                                 
052762       Add 1 to symb-ix                                                   
052763       Move 'RTEGRP'  to symb-parm(symb-ix)                               
052764       Move 'QASE'    to symb-value(symb-ix)                              
052765       Move 4         to symb-len(symb-ix)                                
052766     End-if                                                               
052767                                                                          
052768     If symb-ix <= max-ix                                                 
052769       Add 1 to symb-ix                                                   
052770       Move 'INDLIB2' to symb-parm(symb-ix)                               
052771       Move 'W.QASE'  to symb-value(symb-ix)                              
052772       Move 6         to symb-len(symb-ix)                                
052773     End-if                                                               
052774                                                                          
052775     If symb-ix <= max-ix                                                 
052776       Add 1 to symb-ix                                                   
052777       Move 'INDLIB3' to symb-parm(symb-ix)                               
052778       Move 'W.QASE'  to symb-value(symb-ix)                              
052779       Move 6         to symb-len(symb-ix)                                
052780     End-if                                                               
052781                                                                          
052782     If symb-ix <= max-ix                                                 
052783       Add 1 to symb-ix                                                   
052784       Move 'INDGRND' to symb-parm(symb-ix)                               
052785       Move 'W.QASE'  to symb-value(symb-ix)                              
052786       Move 6         to symb-len(symb-ix)                                
052787     End-if                                                               
052788                                                                          
052789     If symb-ix <= max-ix                                                 
052790       Add 1 to symb-ix                                                   
052791       Move 'GRNDGRP' to symb-parm(symb-ix)                               
052792       Move 'QASE'    to symb-value(symb-ix)                              
052793       Move 4         to symb-len(symb-ix)                                
052794     End-if                                                               
052795                                                                          
052796     If symb-ix <= max-ix                                                 
052797       Add 1 to symb-ix                                                   
052798       Move 'W'       to symb-parm(symb-ix)                               
052799       Move 'W'       to symb-value(symb-ix)                              
052800       Move 1         to symb-len(symb-ix)                                
052801     End-if                                                               
052802                                                                          
052803     If symb-ix <= max-ix                                                 
052804       Add 1 to symb-ix                                                   
052805       Move 'INDBAS'  to symb-parm(symb-ix)                               
052806       Move 'WG01'    to symb-value(symb-ix)                              
052807       Move 4         to symb-len(symb-ix)                                
052808     End-if                                                               
052809                                                                          
052810     If symb-ix <= max-ix                                                 
052811       Add 1 to symb-ix                                                   
052812       Move 'DBB'     to symb-parm(symb-ix)                               
052813       Move 'DBB'     to symb-value(symb-ix)                              
052814       Move 3         to symb-len(symb-ix)                                
052815     End-if                                                               
052816                                                                          
052817     If symb-ix <= max-ix                                                 
052818       Add 1 to symb-ix                                                   
052819       Move 'IMS'     to symb-parm(symb-ix)                               
052820       Move 'IMG0'    to symb-value(symb-ix)                              
052821       Move 4         to symb-len(symb-ix)                                
052822     End-if                                                               
052823                                                                          
052824     If symb-ix <= max-ix                                                 
052825       Add 1 to symb-ix                                                   
052826       Move 'DB2SYS'  to symb-parm(symb-ix)                               
052827       Move 'D2G0'    to symb-value(symb-ix)                              
052828       Move 4         to symb-len(symb-ix)                                
052829     End-if                                                               
052830                                                                          
052831     If symb-ix <= max-ix                                                 
052832       Add 1 to symb-ix                                                   
052833       Move 'CPU'     to symb-parm(symb-ix)                               
052834       Move '1'       to symb-value(symb-ix)                              
052835       Move 1         to symb-len(symb-ix)                                
052836     End-if                                                               
052837                                                                          
052838     If symb-ix <= max-ix                                                 
052839       Add 1 to symb-ix                                                   
052840       Move 'DBRC'    to symb-parm(symb-ix)                               
052841       Move 'Y'       to symb-value(symb-ix)                              
052842       Move 1         to symb-len(symb-ix)                                
052843     End-if                                                               
052844                                                                          
052845     If symb-ix <= max-ix                                                 
052846       Add 1 to symb-ix                                                   
052847       Move 'INDRESL' to symb-parm(symb-ix)                               
052848       Move 'SYS1.F1IM00.IMS'  to symb-value(symb-ix)                     
052849       Move 15        to symb-len(symb-ix)                                
052850     End-if                                                               
052851                                                                          
052852     If symb-ix <= max-ix                                                 
052853       Add 1 to symb-ix                                                   
052854       Move 'INDUSRL' to symb-parm(symb-ix)                               
052855       Move 'F1IMG0.IMS'  to symb-value(symb-ix)                          
052856       Move 10        to symb-len(symb-ix)                                
052857     End-if                                                               
052858                                                                          
052859     If symb-ix <= max-ix                                                 
052860       Add 1 to symb-ix                                                   
052861       Move 'INDPLOG' to symb-parm(symb-ix)                               
052862       Move 'WG01.PGMLOG' to symb-value(symb-ix)                          
052863       Move 11        to symb-len(symb-ix)                                
052864     End-if                                                               
052865                                                                          
052866     If symb-ix <= max-ix                                                 
052867       Add 1 to symb-ix                                                   
052868       Move 'VX'      to symb-parm(symb-ix)                               
052869       Move 'VC'      to symb-value(symb-ix)                              
052870       Move 2         to symb-len(symb-ix)                                
052871     End-if                                                               
052880     .                                                                    
052900     eject                                                                
053000 S55-HITTA-DSNAMN Section.                                                
053100     skip2                                                                
053200     Move 3 To word-number                                                
053300     Call WORDIND Using wordind-parms                                     
053400* Finns det något ord efter DD                                            
053500     If word-pos > 0                                                      
053600       Move word-string(word-pos:word-length) To symb-var-str             
053700       Perform S60-SET-SYMB-VARS                                          
053800       Move symb-var-str To w-str                                         
053900       Evaluate True                                                      
054000         When w-str(1:6) = 'SYSOUT'                                       
054100           Move 'SYSOUT' To ut-dsnamn                                     
054200           Perform S12-SKRIV-UTFIL-02                                     
054300         When w-str(1:5) = 'DUMMY'                                        
054400           Move 'DUMMY' To ut-dsnamn                                      
054500         When w-str(1:7) = 'DDNAME='                                      
054600           Move w-str(8:) To w-str2                                       
054700           Unstring w-str2 Delimited By ',' Or Space                      
054800               Into w-str3                                                
054900           Move Space   To ut-dsnamn                                      
055000           String ut-procedur Delimited By Space                          
055100                  '-'         Delimited By Size                           
055200                  w-str3      Delimited By Space                          
055300             Into ut-dsnamn                                               
055400         When w-str(1:3) = 'DSN'                                          
055500           Move ',' To parm-del                                           
055600           Move w-str to parm-string                                      
055700                                                                          
055800           Call PARMFIND Using parmfind-parms                             
055900           Move w-str(parm-pos:parm-length) To w-str                      
056000           Unstring w-str Delimited by '(' Or All Space                   
056100               Into w-str2                                                
056200               Count In pekare                                            
056300           Evaluate True                                                  
056400*   Om det inte finns någon vänsterparantes                               
056500             When w-str = w-str2                                          
056600               If pekare > 8 And                                          
056700                  w-str(pekare - 1:2) = 'P1' And                          
056800                  w-str(pekare - 8:1) = '.'                               
056900                 Move w-str(pekare - 7:6) To ut-dsnamn                    
057000               Else                                                       
057100                 Move 0 To ix                                             
057200                 Perform Until w-str(pekare - ix:1) = '.' Or              
057300                               w-str(pekare - ix:1) = '='                 
057400                   Add 1 To ix                                            
057500                 End-Perform                                              
057600*   Om dsnman avslutas med '.' eller '='                                  
057700                 If ix > 0                                                
057800                   Move w-str(pekare - ix + 1:ix) To ut-dsnamn            
057900                 Else                                                     
058000                  Display '--Can not find DSN for DDNAME '                
058100                  Display '  ' ut-ddnamn(1:8) ' in ' spar-proc-mbr        
058200                  Move 'DUMMY' To ut-dsnamn                               
058300                 End-if                                                   
058400               End-If                                                     
058500*   Om det är ett generationsdataset                                      
058600             When w-str(pekare + 2:1) = '+' Or                            
058700                  w-str(pekare + 2:1) = '-' Or                            
058800                  w-str(pekare + 2:1) = '0'                               
058900               Move 0 To ix                                               
059000               Perform Until w-str(pekare - ix:1) = '.' Or                
059100                             w-str(pekare - ix:1) = '='                   
059200                 Add 1 To ix                                              
059300               End-Perform                                                
059400               If ix > 0                                                  
059500                 Move w-str(pekare - ix + 1:ix) To ut-dsnamn              
059600               Else                                                       
059700                 Display '--Can not find DSN for DDNAME '                 
059800                 Display '  ' ut-ddnamn(1:8) ' in ' spar-proc-mbr         
059900                 Move 'DUMMY' To ut-dsnamn                                
060000               End-if                                                     
060100*   Om det är något annat. T.ex. PDS                                      
060200             When Other                                                   
060300               Move 0 To ix                                               
060400               Perform Until w-str(pekare - ix:1) = '.' Or                
060500                             w-str(pekare - ix:1) = '='                   
060600                 Add 1 To ix                                              
060700               End-Perform                                                
060800               Move w-str(pekare - ix + 1:) To ut-dsnamn                  
060900*   Fix för generations-PDS:er                                            
061000               If ut-dsnamn(7:3) = 'P1('                                  
061100                 Move Space to w-str                                      
061200                 String ut-dsnamn(1:6) Delimited By Size                  
061300                        ut-dsnamn(9:) Delimited By Size                   
061400                   Into w-str                                             
061500                 Move w-str To ut-dsnamn                                  
061600               End-If                                                     
061700           End-Evaluate                                                   
061800         When Other                                                       
061900           Move Space to ut-dsnamn                                        
062000       End-Evaluate                                                       
062100     Else                                                                 
062200       Move Space to ut-dsnamn                                            
062300     End-If                                                               
062400     .                                                                    
062500     eject                                                                
062600 S60-SET-SYMB-VARS Section.                                               
062700     skip2                                                                
062800     Move symb-var-str to w-str                                           
062900     Move 1 to ix                                                         
063000     Perform Until w-str(ix:) = Space                                     
063100* Är det en variabel som börjar                                           
063200       If w-str(ix:1) = '&'                                               
063300* eller är det en temp-fil                                                
063400         If w-str (ix + 1:1) Not = '&'                                    
063500           Move ix To symb-start                                          
063600           Add 1 To ix                                                    
063700* Räkna fram till ett tecken som avslutar variabeln                       
063800           Perform Until w-str (ix:1) = '.' Or                            
063900                w-str(ix:1) = Space Or                                    
064000               (w-str(ix:1) Not Alphabetic And                            
064100                w-str(ix:1) Not Numeric)                                  
064200             Add 1 To ix                                                  
064300           End-Perform                                                    
064400           Move ix To symb-end                                            
064500* Räkna fram position och längd för variabelnamnet                        
064600           Compute p-start = symb-start + 1                               
064700           Compute p-len   = symb-end - p-start                           
064800* Hoppa över punkt som avslutar en variabel                               
064900           If w-str(symb-end:1) = '.'                                     
065000             Add 1 to symb-end                                            
065100           End-if                                                         
065200* Sök efter värdet på variabeln                                           
065300           Move w-str(p-start:p-len) To w-str2                            
065400           Move 1 to symb-ix                                              
065500           Perform Until symb-parm(symb-ix) = w-str2 Or                   
065600                         symb-parm(symb-ix) = Space                       
065700             Add 1 To symb-ix                                             
065800           End-perform                                                    
065900* Byt ut variabeln till dess värde                                        
066000                                                                          
066100           If symb-parm(symb-ix) Not = Space                              
066200*   Om variabeln har värdet BLANK                                         
066300             If symb-value(symb-ix) = Space                               
066400*     Om det finns text före variabeln                                    
066500               If symb-start > 1                                          
066600                 String w-str(1:symb-start - 1)                           
066700                                 Delimited By Size                        
066800                        w-str(symb-end:)                                  
066900                                 Delimited By Size                        
067000                   Into w-str2                                            
067100*     Det finns ingen text före variabeln                                 
067200               Else                                                       
067300                 String w-str(symb-end:)                                  
067400                                 Delimited By Size                        
067500                   Into w-str2                                            
067600               End-If                                                     
067700*   Om variabeln INTE har värdet BLANK                                    
067800             Else                                                         
067900               Move symb-len(symb-ix) To w-len                            
068000*     Om det finns text före variabeln                                    
068100               If symb-start > 1                                          
068200                 String w-str(1:symb-start - 1)                           
068300                                 Delimited By Size                        
068400                        symb-value(symb-ix) (1:w-len)                     
068500                                 Delimited By Size                        
068600                        w-str(symb-end:)                                  
068700                                 Delimited By Size                        
068800                   Into w-str2                                            
068900*     Det finns ingen text före variabeln                                 
069000               Else                                                       
069100                 String symb-value(symb-ix) (1:w-len)                     
069200                                 Delimited By Size                        
069300                        w-str(symb-end:)                                  
069400                                 Delimited By Size                        
069500                   Into w-str2                                            
069600               End-If                                                     
069700             End-If                                                       
069800             Move w-str2 To w-str                                         
069900*   Sätt ix till första positionen efter den utbytta variabeln            
070000             Compute ix = symb-start - 1 + symb-len(symb-ix) + 1          
070100           Else                                                           
070200             If w-str(symb-start:5) Not = '&AREA'                         
070300      Display '--Symbolic parm for ' w-str(symb-start:p-len + 1)          
070400      Display '  not found in ' spar-proc-mbr                             
070500      Display '  Can be a temporary file.'                                
070600             End-If                                                       
070700           End-If                                                         
070800         Else                                                             
070900           Add +1 To ix                                                   
071000         End-if                                                           
071100       End-if                                                             
071200       Add 1 To ix                                                        
071300     End-Perform                                                          
071400     Move w-str To symb-var-str                                           
071500     .                                                                    
071600     eject                                                                
071700 S99-ABEND Section.                                                       
071800     skip2                                                                
071900     Call ABEND Using rkod-abend-utan-dump                                
072000     .                                                                    
