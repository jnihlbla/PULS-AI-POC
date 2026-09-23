000100 Id Division.                                                             
000200     skip2                                                                
000300 Program-Id.     WDMR0500.                                                
000400*AUTHOR.         ODD OLSEN.                                               
000500*DATE-WRITTEN.   92/01/09.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        Splitta filen från PROCLIB til en fil för vardera                
001100*        procedurer, job-steg, program och dataset.                       
001200*                                                                         
001300*        Rensa bort sådant som inte skall med i Data Manager              
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     skip3                                                                
002100 Environment Division.                                                    
002200     skip2                                                                
002300 Input-Output Section.                                                    
002400                                                                          
002500 File-Control.                                                            
002600     skip2                                                                
002700*          --- INFIL                                                      
002800     Select WDMR02                     Assign To WDMR05D1.                
002900     skip2                                                                
003000*          --- PROCEDURE                                                  
003100     Select WDMR0A                     Assign To WDMR05D2.                
003200     skip2                                                                
003300*          --- JOB STEP                                                   
003400     Select WDMR0B                     Assign To WDMR05D3.                
003500     skip2                                                                
003600*          --- PROGRAM                                                    
003700     Select WDMR0C                     Assign To WDMR05D4.                
003800     skip2                                                                
003900*          --- DATASET                                                    
004000     Select WDMR0D                     Assign To WDMR05D5.                
004100     eject                                                                
004200 Data Division.                                                           
004300     skip3                                                                
004400 File Section.                                                            
004500     skip3                                                                
004600 Fd  WDMR02                                                               
004700     Recording       F                                                    
004800     Block Contains  0.                                                   
004900     skip2                                                                
005000 01  Filler                           Pic X(120).                         
005100     skip3                                                                
005200 Fd  WDMR0A                                                               
005300     Recording       F                                                    
005400     Block Contains  0.                                                   
005500     skip2                                                                
005600 01  proc-post                   Pic X(80).                               
005700     skip3                                                                
005800 Fd  WDMR0B                                                               
005900     Recording       F                                                    
006000     Block Contains  0.                                                   
006100     skip2                                                                
006200 01  job-post                    Pic X(80).                               
006300     skip3                                                                
006400 Fd  WDMR0C                                                               
006500     Recording       F                                                    
006600     Block Contains  0.                                                   
006700     skip2                                                                
006800 01  pgm-post                    Pic X(95).                               
006900     skip3                                                                
007000 Fd  WDMR0D                                                               
007100     Recording       F                                                    
007200     Block Contains  0.                                                   
007300     skip2                                                                
007400 01  dsn-post                    Pic X(80).                               
007500     eject                                                                
007600 Working-Storage Section.                                                 
007700                                                                          
007800*    -- CHECKED BY WY2000                                                 
007900     skip2                                                                
008000 77  idpgm                       Pic X(8)    Value 'WDMR0500'.            
008100 77  ja                          Pic X       Value 'J'.                   
008200 77  nej                         Pic X       Value 'N'.                   
008300 77  fnutt                       Pic X       Value ''''.                  
008400                                                                          
008500 77  rdmr02-eof-sw               Pic X       Value 'N'.                   
008600     88  eof                                 Value 'J'.                   
008700                                                                          
008800 77  proc-sw                     Pic X       Value 'N'.                   
008900     88  new-proc                            Value 'J'.                   
009000     88  old-proc                            Value 'N'.                   
009100                                                                          
009200 77  pgm-sw                      Pic X       Value 'N'.                   
009300     88  new-pgm                             Value 'J'.                   
009400     88  old-pgm                             Value 'N'.                   
009500                                                                          
009600 77  dsn-sw                      Pic X       Value 'N'.                   
009700     88  new-dsn                             Value 'J'.                   
009800     88  old-dsn                             Value 'N'.                   
009900                                                                          
010000*    --- ARBETSVARIABLER                                                  
010100                                                                          
010200 77  w-str                       Pic x(200)  Value Space.                 
010300 77  w-laddmodul                 Pic x(12)   Value Space.                 
010400 77  w-procedur                  Pic x(13)   Value Space.                 
010500 77  spar-procedur               Pic x(8)    Value Space.                 
010600 77  spar-program                Pic x(8)    Value Space.                 
010700 77  spar-std-program            Pic x(12)   Value Space.                 
010800 77  spar-dsnamn                 Pic x(41)   Value Space.                 
010900     eject                                                                
011000 01  dagens-datum                Pic 9(6)    Value Zero.                  
011100 01  Filler Redefines dagens-datum.                                       
011200     03  dagens-datum-aar        Pic 9(2).                                
011300     03  dagens-datum-maanad     Pic 9(2).                                
011400     03  dagens-datum-dag        Pic 9(2).                                
011500     eject                                                                
011600 01  dynamiska-subprogram.                                                
011700*                                                                         
011800     03  abend                   Pic X(8)    Value 'ABEND'.               
011900     03  postsum                 Pic X(8)    Value 'POSTSUM'.             
012000     skip2                                                                
012100*    --- PARAMETRAR TILL ABEND                                            
012200                                                                          
012300 77  rkod-abend-utan-dump        Pic S9(4)   Comp Value +16.              
012400 77  rkod-abend-med-dump         Pic S9(4)   Comp Value +1000.            
012500     skip2                                                                
012600 01  feltext.                                                             
012700     03  Filler                  Pic X(8)    Value 'FELTEXT'.             
012800     03  feltext-str             Pic X(72)   Value Space.                 
012900     eject                                                                
013000*    --- PARAMETRAR TILL POSTSUM                                          
013100*                                                                         
013200*01  -COPY W0005   -PRE  POSTSUM-                                         
013300     eject                                                                
013400*    --- Arbetsareor för indatakontroller                                 
013500*                                                                         
013600 77  pgm-typ-sw                  Pic 9(4) Comp Value Zero.                
013700     88 ej-std-pgm                             Value 0.                   
013800     88 std-pgm-utan-dsnamn                    Value 1.                   
013900     88 std-pgm-med-dsnamn                     Value 2.                   
014000     88 std-proc                               Value 3.                   
014100*                                                                         
014200 77  dsn-typ-sw                  Pic 9(4) Comp Value Zero.                
014300     88 ej-std-dsn                             Value 0.                   
014400     88 std-dsn                                Value 1.                   
014500*                                                                         
014600 77  steg-typ-sw                 Pic 9(4) Comp Value Zero.                
014700     88 ej-std-stegnamn                        Value 0.                   
014800     88 std-stegnamn                           Value 1.                   
014900                                                                          
015000     eject                                                                
015100 01  in-area-start               Pic X(24)   Value                        
015200                                 'IN-AREA-START  '.                       
015300                                                                          
015400 01  in-area.                                                             
015500     03 in-procedur              Pic x(8).                                
015600     03 in-steg                  Pic x(8).                                
015700     03 in-program               Pic x(22).                               
015800     03 in-pgmtyp                Pic x(8).                                
015900     03 in-psb                   Pic x(8).                                
016000     03 in-ddnamn                Pic x(20).                               
016100     03 in-dsnamn                Pic x(41).                               
016200     eject                                                                
016300 01  proc-area-start             Pic X(24)   Value                        
016400                                 'PROC-AREA-START  '.                     
016500                                                                          
016600 01  proc-area.                                                           
016700     03 proc-procedur            Pic x(13).                               
016800     03 proc-steg                Pic x(17).                               
016900     03 proc-program             Pic x(22).                               
017000     eject                                                                
017100 01  job-area-start              Pic X(24)   Value                        
017200                                 'JOB-AREA-START  '.                      
017300                                                                          
017400 01  job-area.                                                            
017500     03 job-steg                 Pic x(17).                               
017600     03 job-program              Pic x(22).                               
017700     eject                                                                
017800 01  pgm-area-start              Pic X(24)   Value                        
017900                                 'PGM-AREA-START  '.                      
018000                                                                          
018100 01  pgm-area.                                                            
018200     03 pgm-program              Pic x(22).                               
018300     03 pgm-pgmtyp               Pic x(8).                                
018400     03 pgm-psb                  Pic x(12).                               
018500     03 pgm-dsnamn               Pic x(41).                               
018600     03 pgm-laddmodul            Pic x(12).                               
018700     eject                                                                
018800 01  dsn-area-start              Pic X(24)   Value                        
018900                                 'DSN-AREA-START  '.                      
019000                                                                          
019100 01  dsn-area.                                                            
019200     03 dsn-dsnamn               Pic x(41).                               
019300     03 dsn-ddnamn               Pic x(20).                               
019400     eject                                                                
019500 Procedure Division.                                                      
019600     skip2                                                                
019700 STYR Section.                                                            
019800                                                                          
019900     Perform A-INIT                                                       
020000     Perform S01-LAES-WDMR02                                              
020100     Perform Until eof                                                    
020200       Perform A1-KONTROLERA-INDATA                                       
020300                                                                          
020400       Evaluate new-proc Also new-pgm Also new-dsn                        
020500         When   True     Also Any     Also Any                            
020600           Perform B-PROCEDUR                                             
020700           Perform C-PROGRAM                                              
020800           Perform D-DSNAMN                                               
020900         When   False    Also True    Also Any                            
021000           Perform B-PROCEDUR                                             
021100           Perform C-PROGRAM                                              
021200           Perform D-DSNAMN                                               
021300         When   False    Also False   Also True                           
021400           Perform C-PROGRAM                                              
021500           Perform D-DSNAMN                                               
021600       End-Evaluate                                                       
021700                                                                          
021800       Perform S01-LAES-WDMR02                                            
021900     End-Perform                                                          
022000                                                                          
022100                                                                          
022200     Perform Z-FINIT                                                      
022300                                                                          
022400     Move Zero To return-code                                             
022500     Goback                                                               
022600     .                                                                    
022700     eject                                                                
022800 A-INIT Section.                                                          
022900                                                                          
023000     Open Input  WDMR02                                                   
023100                                                                          
023200     Open Output WDMR0A                                                   
023300                 WDMR0B                                                   
023400                 WDMR0C                                                   
023500                 WDMR0D                                                   
023600                                                                          
023700     Accept dagens-datum  From Date                                       
023800     Move idpgm To postsum-prognamn                                       
023900                                                                          
024000     .                                                                    
024100     eject                                                                
024200 A1-KONTROLERA-INDATA Section.                                            
024300                                                                          
024400     Perform S51-KOLLA-OM-STANDARD-PROGRAM                                
024500     Perform S53-KOLLA-OM-STANDARD-STEGNAMN                               
024600     Perform S56-KOLLA-OM-STANDARD-DATASET                                
024700                                                                          
024800     If in-procedur Not = spar-procedur                                   
024900       Move in-procedur To spar-procedur                                  
025000       Move Space To w-procedur                                           
025100       String in-procedur Delimited By Space                              
025200              '-PROC'     Delimited By Size                               
025300         Into w-procedur                                                  
025400       Set new-proc To True                                               
025500     Else                                                                 
025600       Set old-proc To True                                               
025700     End-If                                                               
025800                                                                          
025900     If in-program  Not = spar-program                                    
026000       Move in-program  To spar-program                                   
026100       Move Space To w-laddmodul                                          
026200       String in-program Delimited By Space                               
026300              '-PGM'     Delimited By Size                                
026400         Into w-laddmodul                                                 
026500       Set new-pgm  To True                                               
026600     Else                                                                 
026700       Set old-pgm  To True                                               
026800     End-If                                                               
026900                                                                          
027000     If in-dsnamn   Not = spar-dsnamn                                     
027100       Move in-dsnamn   To spar-dsnamn                                    
027200       Set new-dsn  To True                                               
027300     Else                                                                 
027400       Set old-dsn  To True                                               
027500     End-If                                                               
027600     eject                                                                
027700* Fixa datasetnamn                                                        
027800                                                                          
027900     If in-dsnamn(1:2) = '&&'                                             
028000       String in-dsnamn(3:) Delimited By Space                            
028100              '-TEMP'       Delimited By Size                             
028200         Into in-dsnamn                                                   
028300     End-If                                                               
028400                                                                          
028500     If in-dsnamn(1:1) = '&'                                              
028600       String in-dsnamn(2:) Delimited By Space                            
028700              '-TEMP'       Delimited By Size                             
028800         Into in-dsnamn                                                   
028900     End-If                                                               
029000                                                                          
029100     If (in-dsnamn(1:2) = 'WD' Or 'WG' Or 'W0' or 'W6') And               
029200         in-dsnamn(3:1) Is Alphabetic And                                 
029300         in-dsnamn(4:1) Is Numeric                                        
029400       Set std-dsn To True                                                
029500     End-If                                                               
029600                                                                          
029700                                                                          
029800     eject                                                                
029900* Fixa programnamn                                                        
030000                                                                          
030100     Evaluate True                                                        
030200       When in-program = 'DUMMY'                                          
030300         Display '--PROGRAM ' in-program ' in proc ' in-procedur          
030400         Display '  changed to ' in-procedur '-DUMMY'                     
030500         Move Space To in-program                                         
030600         String in-procedur Delimited By Space                            
030700                '-DUMMY'    Delimited By Size                             
030800           Into in-program                                                
030900       When in-program(1:1) = '&'                                         
031000         Display '--PROGRAM ' in-program ' in proc ' in-procedur          
031100         Display '  changed to ' in-procedur '-DUMMY'                     
031200         Move Space To in-program                                         
031300         String in-procedur Delimited By Space                            
031400                '-DUMMY'    Delimited By Size                             
031500           Into in-program                                                
031600       When std-pgm-med-dsnamn                                            
031700         If std-stegnamn or in-program = space                            
031800           Move Space To w-str                                            
031900           String in-steg     Delimited By Space                          
032000                  '-'         Delimited By Size                           
032100                  in-procedur Delimited By Space                          
032200             Into w-str                                                   
032300           Move w-str To in-program                                       
032400         Else                                                             
032500           Move Space To w-str                                            
032600           String in-program  Delimited By Space                          
032700                  '-'         Delimited By Size                           
032800                  in-procedur Delimited By Space                          
032900             Into w-str                                                   
033000           Move w-str To in-program                                       
033100         End-If                                                           
033200       When std-proc                                                      
033300         Move Space To w-str                                              
033400         String in-program  Delimited By Space                            
033500                '-PROC'     Delimited By Size                             
033600           Into w-str                                                     
033700         Move w-str To in-program                                         
033800       When Other                                                         
033900         Move Space To w-str                                              
034000         If in-program = space                                            
034100           String in-steg     Delimited By Space                          
034200                  '-'         Delimited By Size                           
034300                  in-procedur Delimited By Space                          
034400             Into w-str                                                   
034500         Else                                                             
034600           String in-program  Delimited By Space                          
034700                  '-'         Delimited By Size                           
034800                  in-procedur Delimited By Space                          
034900             Into w-str                                                   
035000         End-if                                                           
035100         Move w-str To in-program                                         
035200     End-Evaluate                                                         
035300     .                                                                    
035400     eject                                                                
035500 B-PROCEDUR Section.                                                      
035600                                                                          
035700     Move w-procedur  To proc-procedur                                    
035800     Move Space       To proc-steg                                        
035900     Move in-program  To proc-program                                     
036000     Perform S11-SKRIV-WDMR0A                                             
036100                                                                          
036200     .                                                                    
036300     eject                                                                
036400 C-PROGRAM  Section.                                                      
036500                                                                          
036600     Evaluate True                                                        
036700       When std-pgm-utan-dsnamn                                           
036800         Move in-program  To pgm-program                                  
036900         If in-pgmtyp = Space                                             
037000           Move 'STDPGM'  To pgm-pgmtyp                                   
037100         Else                                                             
037200           Move in-pgmtyp To pgm-pgmtyp                                   
037300         End-If                                                           
037400         Move Space        To pgm-psb                                     
037500         Move Space        To pgm-dsnamn                                  
037600         Move w-laddmodul  To pgm-laddmodul                               
037700         If new-pgm                                                       
037800           Perform S13-SKRIV-WDMR0C                                       
037900         End-If                                                           
038000       When std-pgm-med-dsnamn                                            
038100         Move in-program  To pgm-program                                  
038200         Move w-laddmodul To pgm-laddmodul                                
038300         If in-pgmtyp = Space                                             
038400           Move 'STDPGM' To pgm-pgmtyp                                    
038500         Else                                                             
038600           Move in-pgmtyp To pgm-pgmtyp                                   
038700         End-If                                                           
038800         Move Space       To pgm-psb                                      
038900         If std-dsn                                                       
039000           Move Space       To pgm-dsnamn                                 
039100* Program med blankt dsnamn behöver bara skrivas ut en gång               
039200           If new-pgm                                                     
039300             Perform S13-SKRIV-WDMR0C                                     
039400           End-If                                                         
039500         Else                                                             
039600           If in-dsnamn not = space                                       
039700             Move Space to w-str                                          
039800             String fnutt     Delimited By Size                           
039900                    in-dsnamn Delimited By Space                          
040000                    '-DSN'    Delimited By Size                           
040100                    fnutt     Delimited By Size                           
040200               Into w-str                                                 
040300             Move w-str     To pgm-dsnamn                                 
040400             Perform S13-SKRIV-WDMR0C                                     
040500           End-If                                                         
040600         End-If                                                           
040700       When std-proc                                                      
040800         Continue                                                         
040900       When Other                                                         
041000         Move in-program   To pgm-program                                 
041100         Move in-pgmtyp    To pgm-pgmtyp                                  
041200         Move w-laddmodul  To pgm-laddmodul                               
041300         Move Space       To pgm-psb                                      
041400         If in-psb Not = Space                                            
041500           String in-psb Delimited By Space                               
041600                  '-PSB' Delimited By Size                                
041700             Into pgm-psb                                                 
041800         End-If                                                           
041900         If std-dsn                                                       
042000           Move Space     To pgm-dsnamn                                   
042100* Program med blankt dsnamn behöver bara skrivas ut en gång               
042200           If new-pgm                                                     
042300             Perform S13-SKRIV-WDMR0C                                     
042400           End-If                                                         
042500         Else                                                             
042600           If in-dsnamn not = space                                       
042700             Move Space to w-str                                          
042800             String fnutt     Delimited By Size                           
042900                    in-dsnamn Delimited By Space                          
043000                    '-DSN'    Delimited By Size                           
043100                    fnutt     Delimited By Size                           
043200               Into w-str                                                 
043300             Move w-str       To pgm-dsnamn                               
043400             Perform S13-SKRIV-WDMR0C                                     
043500           Else                                                           
043600             Move Space     To pgm-dsnamn                                 
043700*   Program med blankt dsnamn behöver bara skrivas ut en gång             
043800             If new-pgm                                                   
043900               Perform S13-SKRIV-WDMR0C                                   
044000             End-If                                                       
044100           End-If                                                         
044200         End-If                                                           
044300     End-Evaluate                                                         
044400     .                                                                    
044500     eject                                                                
044600 D-DSNAMN   Section.                                                      
044700                                                                          
044800     If ej-std-dsn                                                        
044900       Evaluate True                                                      
045000         When std-pgm-utan-dsnamn                                         
045100           Continue                                                       
045200         When std-proc                                                    
045300           Continue                                                       
045400         When std-pgm-med-dsnamn                                          
045500           Move Space to w-str                                            
045600           String fnutt     Delimited By Size                             
045700                  in-dsnamn Delimited By Space                            
045800                  '-DSN'    Delimited By Size                             
045900                  fnutt     Delimited By Size                             
046000             Into w-str                                                   
046100           Move w-str     To dsn-dsnamn                                   
046200           Move Space    To dsn-ddnamn                                    
046300           Perform S14-SKRIV-WDMR0D                                       
046400         When Other                                                       
046500           Move Space to w-str                                            
046600           String fnutt     Delimited By Size                             
046700                  in-dsnamn Delimited By Space                            
046800                  '-DSN'    Delimited By Size                             
046900                  fnutt     Delimited By Size                             
047000             Into w-str                                                   
047100           Move w-str     To dsn-dsnamn                                   
047200           Move Space to w-str                                            
047300           String in-ddnamn Delimited By Space                            
047400                  '-DD'     Delimited By Size                             
047500             Into w-str                                                   
047600           Move w-str     To dsn-ddnamn                                   
047700           Perform S14-SKRIV-WDMR0D                                       
047800       End-Evaluate                                                       
047900     End-If                                                               
048000     .                                                                    
048100     eject                                                                
048200 Z-FINIT Section.                                                         
048300                                                                          
048400     Close WDMR02                                                         
048500           WDMR0A                                                         
048600           WDMR0B                                                         
048700           WDMR0C                                                         
048800           WDMR0D                                                         
048900                                                                          
049000     Move 'S' To postsum-opkod                                            
049100     Call POSTSUM Using postsum-parm                                      
049200     .                                                                    
049300     eject                                                                
049400 S01-LAES-WDMR02  Section.                                                
049500                                                                          
049600     Read WDMR02 Into in-area                                             
049700     At End                                                               
049800        Set eof To True                                                   
049900                                                                          
050000     Not At End                                                           
050100        Move 'WDMR02'   To postsum-fdnamn                                 
050200        Move 'WDMR05D1' To postsum-ddnamn2                                
050300        Move Space      To postsum-transtyp                               
050400        Call POSTSUM Using postsum-parm                                   
050500     End-Read                                                             
050600     .                                                                    
050700     eject                                                                
050800 S11-SKRIV-WDMR0A Section.                                                
050900                                                                          
051000     Write proc-post From proc-area                                       
051100                                                                          
051200     Move 'PRC'      To postsum-transtyp                                  
051300     Move 'WDMR0A'   To postsum-fdnamn                                    
051400     Move 'WDMR05D2' To postsum-ddnamn2                                   
051500     Call POSTSUM Using postsum-parm                                      
051600     .                                                                    
051700     eject                                                                
051800 S12-SKRIV-WDMR0B Section.                                                
051900                                                                          
052000     Write job-post From job-area                                         
052100                                                                          
052200     Move 'JOB'      To postsum-transtyp                                  
052300     Move 'WDMR0B'   To postsum-fdnamn                                    
052400     Move 'WDMR05D3' To postsum-ddnamn2                                   
052500     Call POSTSUM Using postsum-parm                                      
052600     .                                                                    
052700     eject                                                                
052800 S13-SKRIV-WDMR0C Section.                                                
052900                                                                          
053000     Write pgm-post From pgm-area                                         
053100                                                                          
053200     Move 'PGM'      To postsum-transtyp                                  
053300     Move 'WDMR0C'   To postsum-fdnamn                                    
053400     Move 'WDMR05D4' To postsum-ddnamn2                                   
053500     Call POSTSUM Using postsum-parm                                      
053600     .                                                                    
053700     eject                                                                
053800 S14-SKRIV-WDMR0D Section.                                                
053900                                                                          
054000     Write dsn-post From dsn-area                                         
054100                                                                          
054200     Move 'DSN'      To postsum-transtyp                                  
054300     Move 'WDMR0D'   To postsum-fdnamn                                    
054400     Move 'WDMR05D5' To postsum-ddnamn2                                   
054500     Call POSTSUM Using postsum-parm                                      
054600     .                                                                    
054700     eject                                                                
054800 S51-KOLLA-OM-STANDARD-PROGRAM Section.                                   
054900                                                                          
055000     Evaluate in-program                                                  
055100       When 'V12501  ' Set std-pgm-utan-dsnamn To True                    
055200       When 'ABEND   ' Set std-pgm-utan-dsnamn To True                    
055300       When 'V12340  ' Set std-pgm-utan-dsnamn To True                    
055400       When 'V16266  ' Set std-pgm-med-dsnamn  To True                    
055500       When 'SORT    ' Set std-pgm-med-dsnamn  To True                    
055600       When 'IEFBR14 ' Set std-pgm-med-dsnamn  To True                    
055700       When 'IEBGENER' Set std-pgm-med-dsnamn  To True                    
055800       When 'ICEGENER' Set std-pgm-med-dsnamn  To True                    
055900       When 'WCOMGENR' Set std-pgm-med-dsnamn  To True                    
056000       When 'V16459  ' Set std-pgm-med-dsnamn  To True                    
056100       When 'IMSFSU  ' Set std-pgm-med-dsnamn  To True                    
056200       When 'IDCAMS  ' Set std-pgm-med-dsnamn  To True                    
056300       When 'IKJEFT01' Set std-pgm-med-dsnamn  To True                    
056400       When 'WBMPSERV' Set std-pgm-med-dsnamn  To True                    
056500       When 'WEMPDEL'  Set std-proc            To True                    
056600       When 'WFTP'     Set std-proc            To True                    
056700       When 'WMEMOSND' Set std-proc            To True                    
056800       When 'WPARM2DS' Set std-proc            To True                    
056900       When Other      Set ej-std-pgm          To True                    
057000     End-Evaluate                                                         
057100     .                                                                    
057200     eject                                                                
057300 S53-KOLLA-OM-STANDARD-STEGNAMN Section.                                  
057400                                                                          
057500     If (in-steg(1:1) = 'W'  And in-steg(2:4) Is Numeric)                 
057600     Or (in-steg(1:4) = 'WDMR')                                           
057700     Or (in-steg(1:2) = 'WF' And in-steg(3:3) Is Numeric)                 
057800     Or (in-steg(1:2) = 'WG' And in-steg(3:3) Is Numeric)                 
057900     Or (in-steg(1:2) = 'WL' And in-steg(3:3) Is Numeric)                 
058000     Or (in-steg(1:2) = 'WZ' And in-steg(3:3) Is Numeric)                 
058100     Or (in-steg(1:4) = 'WXTR')                                           
058200       Set std-stegnamn    To True                                        
058300     Else                                                                 
058400       Set ej-std-stegnamn To True                                        
058500     End-If                                                               
058600     .                                                                    
058700     eject                                                                
058800 S56-KOLLA-OM-STANDARD-DATASET Section.                                   
058900                                                                          
059000     Evaluate in-dsnamn                                                   
059100       When 'LOAD    '   Set std-dsn    To True                           
059200       When 'LOAD'''     Set std-dsn    To True                           
059300       Display '--LOAD with apostrophe in procedure ' in-procedur         
059400       When 'FADUMP  '   Set std-dsn    To True                           
059500       When 'PSBLIB  '   Set std-dsn    To True                           
059600       When 'DBDLIB  '   Set std-dsn    To True                           
059700       When 'ACBLIB  '   Set std-dsn    To True                           
059800       When 'SYSOUT  '   Set std-dsn    To True                           
059900       When 'DUMMY   '   Set std-dsn    To True                           
060000       When 'DUMMYP1 '   Set std-dsn    To True                           
060100         Display '--DUMMYP1-dsn in procedure ' in-procedur                
060200       When 'NULLFILE'   Set std-dsn    To True                           
060300         Display '--NULLFILE-dsn in procedure ' in-procedur               
060400       When 'NULLFILEP1' Set std-dsn    To True                           
060500         Display '--NULLFILEP1-dsn in procedure ' in-procedur             
060600*      When 'DATUM   '   Set std-dsn    To True                           
060700*      When 'DATWEEK '   Set std-dsn    To True                           
060800       When 'SUBRTNES'   Set std-dsn    To True                           
060900       When 'LOADSERV'   Set std-dsn    To True                           
061000       When 'USERLIB '   Set std-dsn    To True                           
061100       When 'SOP     '   Set std-dsn    To True                           
061200       When Other        Set ej-std-dsn To True                           
061300     End-Evaluate                                                         
061400     .                                                                    
