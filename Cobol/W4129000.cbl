000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4129000.                                                
000300 AUTHOR.         LENA BROMANDER.                                          
000400 DATE-WRITTEN.   16/04/04.                                                
000500                                                                          
000600                                                                          
000700     REMARKS.                                                             
000800*    FUNKTION:                                                            
000900*        KOLLAR MOT WDB2 VILKA DISTRIKT SOM SKALL FÅ VIPS TRANSAR         
001000*                                                                         
001100*        PROGRAMMET LÄSER     WLGMTA (WDB2)                               
001200*                                                                         
001300*        SKAPAR EN UTFIL PER MARKNAD                                      
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . . RETURKOD FRÅN SORTERING                         
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- VIPS TRANSAR IN                                            
002700     SELECT W41229                     ASSIGN TO W41290D1.                
002800     SKIP2                                                                
002900*          --- VIPS TRANSAR UT                                            
003000*                        SVERIGE (SVENSKA BIL)                            
003100     SELECT W41290SE                   ASSIGN TO W41290D2.                
003200*                        NORGE                                            
003300     SELECT W41290NO                   ASSIGN TO W41290D3.                
003400*                        DANMARK                                          
003500     SELECT W41290DK                   ASSIGN TO W41290D4.                
003600*                        FINLAND                                          
003700     SELECT W41290FI                   ASSIGN TO W41290D5.                
003800*                        BELGIEN                                          
003900     SELECT W41290BE                   ASSIGN TO W41290D6.                
004000*                        ENGLAND                                          
004100     SELECT W41290GB                   ASSIGN TO W41290D7.                
004200*                        FRANKRIKE                                        
004300     SELECT W41290FR                   ASSIGN TO W41290D8.                
004400*                        HOLLAND                                          
004500     SELECT W41290NL                   ASSIGN TO W41290D9.                
004600*                        IRLAND                                           
004700     SELECT W41290EI                   ASSIGN TO W41290DA.                
004800*                        ITALIEN                                          
004900     SELECT W41290IT                   ASSIGN TO W41290DB.                
005000*                        PORTUGAL                                         
005100     SELECT W41290PT                   ASSIGN TO W41290DC.                
005200*                        PORTUGAL2                                        
005300     SELECT W41290PX                   ASSIGN TO W41290DD.                
005400*                        SCHWEIZ                                          
005500     SELECT W41290CH                   ASSIGN TO W41290DE.                
005600*                        SPANIEN                                          
005700     SELECT W41290ES                   ASSIGN TO W41290DF.                
005800*                        TYSKLAND                                         
005900     SELECT W41290DE                   ASSIGN TO W41290DG.                
006000*                        ÖSTERRIKE                                        
006100     SELECT W41290AT                   ASSIGN TO W41290DH.                
006200*                        POLEN                                            
006300     SELECT W41290PL                   ASSIGN TO W41290DI.                
006400*                        SAUDIARABIEN                                     
006500     SELECT W41290SA                   ASSIGN TO W41290DJ.                
006600*                        MALAYSIA                                         
006700     SELECT W41290MY                   ASSIGN TO W41290DK.                
006800*                        TURKIET                                          
006900     SELECT W41290TR                   ASSIGN TO W41290DL.                
007000*                        KOREA(SYD-)                                      
007100     SELECT W41290KR                   ASSIGN TO W41290DM.                
007200*                        TAIWAN                                           
007300     SELECT W41290TX                   ASSIGN TO W41290DN.                
007400*                        TAIWAN2                                          
007500     SELECT W41290TW                   ASSIGN TO W41290DO.                
007600*                        MEXIKO                                           
007700     SELECT W41290MX                   ASSIGN TO W41290DP.                
007800*                        BRASILIEN                                        
007900     SELECT W41290BR                   ASSIGN TO W41290DQ.                
008000*                        CANADA                                           
008100     SELECT W41290CA                   ASSIGN TO W41290DR.                
008200*                        JAPAN                                            
008300     SELECT W41290JP                   ASSIGN TO W41290DS.                
008400*                        UNGERN                                           
008500     SELECT W41290HU                   ASSIGN TO W41290DT.                
008600*                        KINA-C1                                          
008700     SELECT W41290C1                   ASSIGN TO W41290DU.                
008800*                        PERU                                             
008900     SELECT W41290PE                   ASSIGN TO W41290DV.                
009000*                        RYSSLAND                                         
009100     SELECT W41290RU                   ASSIGN TO W41290DW.                
009200*                        SYDAFRIKA                                        
009300     SELECT W41290ZA                   ASSIGN TO W41290DX.                
009400*                        USA                                              
009500     SELECT W41290US                   ASSIGN TO W41290DY.                
009610*                        TJECKIEN                                         
009620     SELECT W41290CZ                   ASSIGN TO W41290DZ.                
009630*                        INDIEN                                           
009640     SELECT W41290IN                   ASSIGN TO W41290E1.                
009650*                        THAILAND                                         
009660     SELECT W41290TH                   ASSIGN TO W41290E2.                
009670*                        AUSTRALIA                                        
009680     SELECT W41290AU                   ASSIGN TO W41290E3.                
009700     SKIP2                                                                
009800* ÖVRIGA MARKNADERS FILER LÄGGS TILL EFTERHAND....                        
009900     SKIP2                                                                
010000*          --- SORTERINGSFIL                                              
010100     SELECT SORTFIL                    ASSIGN TO W41290DS.                
010200     EJECT                                                                
010300 DATA DIVISION.                                                           
010400     SKIP3                                                                
010500 FILE SECTION.                                                            
010600     SKIP3                                                                
010700 FD  W41229                                                               
010800     LABEL RECORD    STANDARD                                             
010900     RECORDING       F                                                    
011000     BLOCK CONTAINS  0.                                                   
011100     SKIP2                                                                
011200*01  -COPY W46333          -L.                                            
011300                                                                          
011400                                                                          
011500     SKIP3                                                                
011600                                                                          
011700 FD  W41290SE                                                             
011800     LABEL RECORD    STANDARD                                             
011900     RECORDING       F                                                    
012000     BLOCK CONTAINS  0.                                                   
012100                                                                          
012200  01  UTPOST-SE      PIC X(211).                                          
012800                                                                          
012900 FD  W41290NO                                                             
013000     LABEL RECORD    STANDARD                                             
013100     RECORDING       F                                                    
013200     BLOCK CONTAINS  0.                                                   
013300                                                                          
013400  01  UTPOST-NO      PIC X(211).                                          
013900                                                                          
014000                                                                          
014100 FD  W41290DK                                                             
014200     LABEL RECORD    STANDARD                                             
014300     RECORDING       F                                                    
014400     BLOCK CONTAINS  0.                                                   
014500     SKIP2                                                                
014600                                                                          
014700  01  UTPOST-DK      PIC X(211).                                          
015200                                                                          
015300 FD  W41290FI                                                             
015400     LABEL RECORD    STANDARD                                             
015500     RECORDING       F                                                    
015600     BLOCK CONTAINS  0.                                                   
015700     SKIP2                                                                
015800                                                                          
015900  01  UTPOST-FI      PIC X(211).                                          
016400                                                                          
016500 FD  W41290BE                                                             
016600     LABEL RECORD    STANDARD                                             
016700     RECORDING       F                                                    
016800     BLOCK CONTAINS  0.                                                   
016900     SKIP2                                                                
017000                                                                          
017100  01  UTPOST-BE      PIC X(211).                                          
017600                                                                          
017700 FD  W41290GB                                                             
017800     LABEL RECORD    STANDARD                                             
017900     RECORDING       F                                                    
018000     BLOCK CONTAINS  0.                                                   
018100     SKIP2                                                                
018200                                                                          
018300  01  UTPOST-GB      PIC X(211).                                          
018800                                                                          
018900 FD  W41290FR                                                             
019000     LABEL RECORD    STANDARD                                             
019100     RECORDING       F                                                    
019200     BLOCK CONTAINS  0.                                                   
019300     SKIP2                                                                
019400                                                                          
019500  01  UTPOST-FR      PIC X(211).                                          
020000                                                                          
020100 FD  W41290NL                                                             
020200     LABEL RECORD    STANDARD                                             
020300     RECORDING       F                                                    
020400     BLOCK CONTAINS  0.                                                   
020500     SKIP2                                                                
020600                                                                          
020700  01  UTPOST-NL      PIC X(211).                                          
021200                                                                          
021300 FD  W41290EI                                                             
021400     LABEL RECORD    STANDARD                                             
021500     RECORDING       F                                                    
021600     BLOCK CONTAINS  0.                                                   
021700     SKIP2                                                                
021800                                                                          
021900  01  UTPOST-EI      PIC X(211).                                          
022400                                                                          
022500 FD  W41290IT                                                             
022600     LABEL RECORD    STANDARD                                             
022700     RECORDING       F                                                    
022800     BLOCK CONTAINS  0.                                                   
022900     SKIP2                                                                
023000                                                                          
023100  01  UTPOST-IT      PIC X(211).                                          
023600                                                                          
023700 FD  W41290PT                                                             
023800     LABEL RECORD    STANDARD                                             
023900     RECORDING       F                                                    
024000     BLOCK CONTAINS  0.                                                   
024100     SKIP2                                                                
024200                                                                          
024300  01  UTPOST-PT      PIC X(211).                                          
024800                                                                          
024900 FD  W41290PX                                                             
025000     LABEL RECORD    STANDARD                                             
025100     RECORDING       F                                                    
025200     BLOCK CONTAINS  0.                                                   
025300     SKIP2                                                                
025400                                                                          
025500  01  UTPOST-PX      PIC X(211).                                          
026000                                                                          
026100 FD  W41290CH                                                             
026200     LABEL RECORD    STANDARD                                             
026300     RECORDING       F                                                    
026400     BLOCK CONTAINS  0.                                                   
026500     SKIP2                                                                
026600                                                                          
026700  01  UTPOST-CH      PIC X(211).                                          
027200                                                                          
027300 FD  W41290ES                                                             
027400     LABEL RECORD    STANDARD                                             
027500     RECORDING       F                                                    
027600     BLOCK CONTAINS  0.                                                   
027700     SKIP2                                                                
027800                                                                          
027900  01  UTPOST-ES      PIC X(211).                                          
028400                                                                          
028500 FD  W41290DE                                                             
028600     LABEL RECORD    STANDARD                                             
028700     RECORDING       F                                                    
028800     BLOCK CONTAINS  0.                                                   
028900     SKIP2                                                                
029000                                                                          
029100  01  UTPOST-DE      PIC X(211).                                          
029600                                                                          
029700 FD  W41290AT                                                             
029800     LABEL RECORD    STANDARD                                             
029900     RECORDING       F                                                    
030000     BLOCK CONTAINS  0.                                                   
030100     SKIP2                                                                
030200                                                                          
030300  01  UTPOST-AT      PIC X(211).                                          
030800                                                                          
030900 FD  W41290PL                                                             
031000     LABEL RECORD    STANDARD                                             
031100     RECORDING       F                                                    
031200     BLOCK CONTAINS  0.                                                   
031300     SKIP2                                                                
031400                                                                          
031500  01  UTPOST-PL      PIC X(211).                                          
031600                                                                          
031700 FD  W41290SA                                                             
031800     LABEL RECORD    STANDARD                                             
031900     RECORDING       V                                                    
032000     BLOCK CONTAINS  0.                                                   
032100     SKIP2                                                                
032200                                                                          
032300  01  UTPOST-SA.                                                          
032400 *    03 -COPY W461RIO2 -L.                                               
032500 *    03 -COPY W461RIM0 -L.                                               
032600 *    03 -COPY W461RIK0 -L.                                               
032700 *    03 -COPY W461RIN0 -L.                                               
032800                                                                          
032900 FD  W41290MY                                                             
033000     LABEL RECORD    STANDARD                                             
033100     RECORDING       F                                                    
033200     BLOCK CONTAINS  0.                                                   
033300     SKIP2                                                                
033400                                                                          
033500  01  UTPOST-MY      PIC X(211).                                          
034000                                                                          
034100 FD  W41290TR                                                             
034200     LABEL RECORD    STANDARD                                             
034300     RECORDING       F                                                    
034400     BLOCK CONTAINS  0.                                                   
034500     SKIP2                                                                
034600                                                                          
034700  01  UTPOST-TR      PIC X(211).                                          
035200                                                                          
035300 FD  W41290KR                                                             
035400     LABEL RECORD    STANDARD                                             
035500     RECORDING       F                                                    
035600     BLOCK CONTAINS  0.                                                   
035700     SKIP2                                                                
035800                                                                          
035900  01  UTPOST-KR      PIC X(211).                                          
036400                                                                          
036500 FD  W41290TX                                                             
036600     LABEL RECORD    STANDARD                                             
036700     RECORDING       F                                                    
036800     BLOCK CONTAINS  0.                                                   
036900     SKIP2                                                                
037000                                                                          
037100  01  UTPOST-TX      PIC X(211).                                          
037600                                                                          
037700 FD  W41290TW                                                             
037800     LABEL RECORD    STANDARD                                             
037900     RECORDING       F                                                    
038000     BLOCK CONTAINS  0.                                                   
038100     SKIP2                                                                
038200                                                                          
038300  01  UTPOST-TW      PIC X(211).                                          
038800                                                                          
038900 FD  W41290MX                                                             
039000     LABEL RECORD    STANDARD                                             
039100     RECORDING       F                                                    
039200     BLOCK CONTAINS  0.                                                   
039300     SKIP2                                                                
039400                                                                          
039500  01  UTPOST-MX      PIC X(211).                                          
040000                                                                          
040100 FD  W41290BR                                                             
040200     LABEL RECORD    STANDARD                                             
040300     RECORDING       F                                                    
040400     BLOCK CONTAINS  0.                                                   
040500     SKIP2                                                                
040600                                                                          
040700  01  UTPOST-BR      PIC X(211).                                          
041200                                                                          
041300 FD  W41290CA                                                             
041400     LABEL RECORD    STANDARD                                             
041500     RECORDING       F                                                    
041600     BLOCK CONTAINS  0.                                                   
041700     SKIP2                                                                
041800                                                                          
041900  01  UTPOST-CA      PIC X(211).                                          
042400                                                                          
042500 FD  W41290JP                                                             
042600     LABEL RECORD    STANDARD                                             
042700     RECORDING       F                                                    
042800     BLOCK CONTAINS  0.                                                   
042900     SKIP2                                                                
043000                                                                          
043100  01  UTPOST-JP      PIC X(211).                                          
043600                                                                          
044900 FD  W41290C1                                                             
045000     LABEL RECORD    STANDARD                                             
045100     RECORDING       F                                                    
045200     BLOCK CONTAINS  0.                                                   
045300     SKIP2                                                                
045400                                                                          
045500  01  UTPOST-C1      PIC X(211).                                          
046000                                                                          
046100 FD  W41290PE                                                             
046200     LABEL RECORD    STANDARD                                             
046300     RECORDING       F                                                    
046400     BLOCK CONTAINS  0.                                                   
046500     SKIP2                                                                
046600                                                                          
046700  01  UTPOST-PE      PIC X(211).                                          
047200                                                                          
047300 FD  W41290RU                                                             
047400     LABEL RECORD    STANDARD                                             
047500     RECORDING       F                                                    
047600     BLOCK CONTAINS  0.                                                   
047700     SKIP2                                                                
047800                                                                          
047900  01  UTPOST-RU      PIC X(211).                                          
048400                                                                          
048500 FD  W41290ZA                                                             
048600     LABEL RECORD    STANDARD                                             
048700     RECORDING       F                                                    
048800     BLOCK CONTAINS  0.                                                   
048900     SKIP2                                                                
049000                                                                          
049100  01  UTPOST-ZA      PIC X(211).                                          
049600                                                                          
049700 FD  W41290US                                                             
049800     LABEL RECORD    STANDARD                                             
049900     RECORDING       F                                                    
050000     BLOCK CONTAINS  0.                                                   
050100     SKIP2                                                                
050200                                                                          
050300  01  UTPOST-US      PIC X(211).                                          
050800                                                                          
050900                                                                          
050910 FD  W41290CZ                                                             
050920     LABEL RECORD    STANDARD                                             
050930     RECORDING       F                                                    
050940     BLOCK CONTAINS  0.                                                   
050950     SKIP2                                                                
050960                                                                          
050970  01  UTPOST-CZ      PIC X(211).                                          
050980                                                                          
050981 FD  W41290HU                                                             
050982     LABEL RECORD    STANDARD                                             
050983     RECORDING       F                                                    
050984     BLOCK CONTAINS  0.                                                   
050985     SKIP2                                                                
050986                                                                          
050987  01  UTPOST-HU      PIC X(211).                                          
050988                                                                          
050989 FD  W41290IN                                                             
050990     LABEL RECORD    STANDARD                                             
050991     RECORDING       F                                                    
050992     BLOCK CONTAINS  0.                                                   
050993     SKIP2                                                                
050994                                                                          
050995  01  UTPOST-IN      PIC X(211).                                          
050996                                                                          
050997 FD  W41290TH                                                             
050998     LABEL RECORD    STANDARD                                             
050999     RECORDING       F                                                    
051000     BLOCK CONTAINS  0.                                                   
051001     SKIP2                                                                
051002                                                                          
051003  01  UTPOST-TH      PIC X(211).                                          
051004                                                                          
051005 FD  W41290AU                                                             
051006     LABEL RECORD    STANDARD                                             
051007     RECORDING       F                                                    
051008     BLOCK CONTAINS  0.                                                   
051009     SKIP2                                                                
051010                                                                          
051011  01  UTPOST-AU      PIC X(211).                                          
051013                                                                          
051020     SKIP3                                                                
051100 SD  SORTFIL                                                              
051200     RECORDING F                                                          
051300     SKIP2                                                                
051400 01  SORT-POST.                                                           
051500**** 03   FILLER                  PIC  X(4).                              
051600**** 03   SORT-IDDISTR            PIC  9(5).                              
051700*03  -COPY W46333  -PRE SORT-                                             
051800                                                                          
051900                                                                          
052000     EJECT                                                                
052100 WORKING-STORAGE SECTION.                                                 
052200     SKIP2                                                                
052300                                                                          
052400*    -- CHECKED BY WY2000                                                 
052500*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
052600 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4129000'.            
052700 77  IDPGM                       PIC X(8)    VALUE 'W4129000'.            
052800 77  JA                          PIC X       VALUE 'J'.                   
052900 77  NEJ                         PIC X       VALUE 'N'.                   
053000 77  W-OLD-IDDISTR               PIC 9(5).                                
053010 77  W-OLD-VIPS-IDDISTR          PIC 9(5).                                
053100 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
053200                                                                          
053300 77  W41229-EOF-SW               PIC X       VALUE 'N'.                   
053400     88  END-OF-W41229                       VALUE 'J'.                   
053500                                                                          
053600 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
053700     88  END-OF-SORTFIL                      VALUE 'J'.                   
053800     EJECT                                                                
053900 01  DAGENS-DATUM.                                                        
054000   03  DAGENS-DATUM-AR           PIC 9(2).                                
054100   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
054200   03  DAGENS-DATUM-DAG          PIC 9(2).                                
054300                                                                          
054400 01  DAGENS-TID.                                                          
054500   03  DAGENS-TID-TIM            PIC 9(2).                                
054600   03  DAGENS-TID-MIN            PIC 9(2).                                
054700   03  DAGENS-TID-SEK            PIC 9(2).                                
054800*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
054900                                                                          
055000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
055100     SKIP2                                                                
055200*01  -COPY WDATKORT                                                       
055300     EJECT                                                                
055400                                                                          
055500 01  FELTEXT.                                                             
055600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
055700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
055800 01  W-SLUTPOST-IDLANDX2         PIC X(2)    VALUE SPACE.                 
055810 01  W-IDDC-11                   PIC X(2)    VALUE '11'.                  
055900 01  W-KDCLAGER-1                PIC 9       VALUE 1.                     
056000 01  W-KDFAKTYP-S                PIC X       VALUE 'S'.                   
056100 01  W-KDFRAKT-88                PIC 99      VALUE 88.                    
056110 01  W-PRKURS-1                  PIC 9(6)V9(5) VALUE 1.                   
056200     EJECT                                                                
056300     EJECT                                                                
056400****************************************************************          
056500*                DISTRIKTSTEST-COPYTEXT                                   
056600****************************************************************          
056700*                                                                         
056800 01  FILLER         PIC X(16)    VALUE 'DISTRIKTSAREA   '.                
056900     SKIP2                                                                
057000 01  TEST-IDDISTR   PIC 9(5)    COMP-3.                                   
057100     SKIP2                                                                
057200*01  FILLER        -COPY WWDIS130   -RED TEST-IDDISTR.                    
057300     EJECT                                                                
057400 01  DYNAMISKA-SUBPROGRAM.                                                
057500*                                                                         
057600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
057700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
057800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
057900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
058000     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
058100     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
058200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
058300     SKIP2                                                                
058400*    --- PARAMETRAR TILL ABEND                                            
058500                                                                          
058600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
058700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
058800     EJECT                                                                
058900 01  FILLER                     PIC X(16)   VALUE 'CIA-AREA'.             
059000*                                                                         
059100*   -COPY W009CIA                                                         
059200*                                                                         
059300*    --- PARAMETRAR TILL POSTSUM                                          
059400*                                                                         
059500*01  -COPY W0005      -PRE  POSTSUM-                                      
059600     EJECT                                                                
059700*    --- PARAMETRAR TILL W460DIS1                                         
059800*                                                                         
059900*01  -COPY W460DIS1                                                       
060000     EJECT                                                                
060100*01  -COPY W460LISO                                                       
060200                                                                          
060300     EJECT                                                                
060400 01  W41290-AREA-START           PIC X(24)   VALUE                        
060500                                 'W41290-AREA-START  '.                   
060600     SKIP2                                                                
060700 01  W41229-INAREA.                                                       
060800*03  -COPY W46333          -L.                                            
060900                                                                          
061000 01  FILLER                      PIC X(24)   VALUE                        
061100                                 'W461RI0N-START  '.                      
061200*01  -COPY W461RI0N     -PRE RI0N-                                        
061300                                                                          
061301 01  FILLER                      PIC X(24)   VALUE                        
061302                                 'W461RI0-START  '.                       
061303*01  -COPY W461RI0      -PRE RI0-                                         
061304                                                                          
061305 01  FILLER                      PIC X(24)   VALUE                        
061306                                 'W461RI9-START  '.                       
061307*01  -COPY W461RI9      -PRE RI9-                                         
061308                                                                          
061310 01  FILLER                      PIC X(24)   VALUE                        
061320                                 'W461RIK0-START  '.                      
061330*01  -COPY W461RIK0     -PRE RIK0-                                        
061340                                                                          
061400 01  FILLER                      PIC X(24)   VALUE                        
061500                                 'W461RIK1-START   '.                     
061600*01  -COPY W461RIK1     -PRE RIK1-                                        
061700                                                                          
061800 01  FILLER                      PIC X(24)   VALUE                        
061900                                 'W461RILN-START   '.                     
062000*01  -COPY W461RILN     -PRE RILN-                                        
062100                                                                          
062200 01  FILLER                      PIC X(24)   VALUE                        
062300                                 'W461RIM0-START  '.                      
062400*01  -COPY W461RIM0     -PRE RIM0-                                        
062500                                                                          
062600 01  FILLER                      PIC X(24)   VALUE                        
062700                                 'W461RIM2-START   '.                     
062800*01  -COPY W461RIM2     -PRE RIM2-                                        
062900                                                                          
063000 01  FILLER                      PIC X(24)   VALUE                        
063100                                 'W461RIN0-START  '.                      
063200*01  -COPY W461RIN0     -PRE RIN0-                                        
063300                                                                          
063400 01  FILLER                      PIC X(24)   VALUE                        
063500                                 'W461RIO2-START   '.                     
063600*01  -COPY W461RIO2     -PRE RIO2-                                        
063700                                                                          
065500 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
065600                                  'SORTWS-AREA-START  '.                  
065700     SKIP2                                                                
065800 01  SORTWS-AREA.                                                         
065900     03  FILLER                  PIC X(4).                                
066000     03  SORTWS-IDDISTR          PIC 9(4).                                
066100     03  SORTWS-IDKUNDNR         PIC 9(6).                                
066200     03  FILLER                  PIC X(115).                              
066300     EJECT                                                                
066400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
066500*                                                                         
066600     EJECT                                                                
066700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
066800     SKIP3                                                                
066900 01  NYCKLAR-TILL-DLI.                                                    
067000     03  W-IDGMT-KEY.                                                     
067100         05  W-IDDISTR           PIC S9(5)      COMP-3.                   
067200         05  W-IDKUNDNR          PIC S9(7)      COMP-3.                   
067300                                                                          
067400     SKIP2                                                                
067500*    --- STATUS-KOD FRÅN IMS                                              
067600 01  STATUS-WS                   PIC XX.                                  
067700     88  SEGMENT-FINNS                       VALUE '  '.                  
067800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
067900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
068000     SKIP2                                                                
068100 01  GODK-STATUSKODER.                                                    
068200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
068300     SKIP3                                                                
068400 01  SSA1                        PIC X(64).                               
068500 01  SSA2                        PIC X(64).                               
068600     EJECT                                                                
068700*    --- IMS FUNKTIONSKODER                                               
068800*01  -COPY W0003                                                          
068900     EJECT                                                                
069000*    ---  DLI INPUT-OUTPUT AREA                                           
069100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
069200     SKIP3                                                                
069300 01  DLI-IO-AREA.                                                         
069400     03  WLGMTA01.                                                        
069500*        05  -COPY WDB201                                                 
069600     EJECT                                                                
069700 LINKAGE SECTION.                                                         
069800                                                                          
069900     EJECT                                                                
070000*01  -COPY W0008      -PRE GMTA-                                          
070100     05  FILLER                  PIC X.                                   
070200     EJECT                                                                
070300 PROCEDURE DIVISION  USING GMTA-PCB.                                      
070400     ENTRY 'DLITCBL' USING GMTA-PCB.                                      
070500                                                                          
070600     SKIP2                                                                
070700     PERFORM A-INIT                                                       
070800                                                                          
070900     SORT SORTFIL ASCENDING KEY SORT-IDDISTR                              
071000                  INPUT PROCEDURE B-BEHANDLA-INFIL                        
071100                  OUTPUT PROCEDURE C-KOLLA-IDDISTR-SKRIV-UTFIL            
071200                                                                          
071300     IF SORT-RETURN NOT = 0                                               
071400       DISPLAY 'RETURKOD ' SORT-RETURN ' FRÅN SORT'                       
071500       PERFORM S99-ABEND                                                  
071600     ELSE                                                                 
071700       PERFORM Z-FINIT                                                    
071800                                                                          
071900       MOVE ZERO TO RETURN-CODE                                           
072000       GOBACK                                                             
072100     END-IF                                                               
072200                                                                          
072300     .                                                                    
072400     EJECT                                                                
072500 A-INIT SECTION.                                                          
072600                                                                          
072700     OPEN INPUT  W41229                                                   
072800                                                                          
072900     OPEN OUTPUT                                                          
073000                 W41290SE                                                 
073100                 W41290NO                                                 
073200                 W41290DK                                                 
073300                 W41290FI                                                 
073400                 W41290BE                                                 
073500                 W41290GB                                                 
073600                 W41290FR                                                 
073700                 W41290NL                                                 
073800                 W41290EI                                                 
073900                 W41290IT                                                 
074000                 W41290PT                                                 
074100                 W41290PX                                                 
074200                 W41290CH                                                 
074300                 W41290ES                                                 
074400                 W41290DE                                                 
074500                 W41290AT                                                 
074600                 W41290PL                                                 
074700                 W41290SA                                                 
074800                 W41290MY                                                 
074900                 W41290TR                                                 
075000                 W41290KR                                                 
075100                 W41290TX                                                 
075200                 W41290TW                                                 
075300                 W41290MX                                                 
075400                 W41290BR                                                 
075500                 W41290CA                                                 
075600                 W41290JP                                                 
075800                 W41290C1                                                 
075900                 W41290PE                                                 
076000                 W41290RU                                                 
076100                 W41290ZA                                                 
076200                 W41290US                                                 
076210                 W41290CZ                                                 
076220                 W41290HU                                                 
076230                 W41290IN                                                 
076240                 W41290TH                                                 
076250                 W41290AU                                                 
076300                                                                          
076400     SKIP2                                                                
076500*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
076600     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
076700     MOVE D-AAR TO DAGENS-DATUM-AR                                        
076800     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
076900     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
077000                                                                          
077100*    ACCEPT DAGENS-DATUM        FROM DATE                                 
077200                                                                          
077300     ACCEPT   DAGENS-TID FROM TIME                                        
077400     MOVE IDPGM                 TO POSTSUM-PROGNAMN                       
077500     .                                                                    
077600     EJECT                                                                
077700 B-BEHANDLA-INFIL   SECTION.                                              
077800                                                                          
077900     PERFORM S01-LAES-W41229                                              
078000     PERFORM UNTIL END-OF-W41229                                          
078100         MOVE W41229-INAREA      TO  SORTWS-AREA                          
078200         PERFORM S31-SORT-RELEASE                                         
078300         PERFORM S01-LAES-W41229                                          
078400     END-PERFORM                                                          
078500     .                                                                    
078600     EJECT                                                                
078700 C-KOLLA-IDDISTR-SKRIV-UTFIL  SECTION.                                    
078800                                                                          
078900     MOVE ZERO                 TO  W-OLD-IDDISTR                          
078910                                   W-OLD-VIPS-IDDISTR                     
079000     PERFORM S32-SORT-RETURN                                              
079100     PERFORM UNTIL END-OF-SORTFIL                                         
079200         IF SORTWS-IDDISTR     =   W-OLD-IDDISTR                          
079300             CONTINUE                                                     
079400          ELSE                                                            
079600             MOVE SORTWS-IDDISTR  TO  W-IDDISTR                           
079700             MOVE SORTWS-IDKUNDNR TO  W-IDKUNDNR                          
079800             PERFORM IMS-GU-GMTA01                                        
079900             MOVE SORTWS-IDDISTR  TO  W-OLD-IDDISTR                       
080000         END-IF                                                           
080100                                                                          
080200         MOVE SORTWS-IDDISTR   TO  TEST-IDDISTR DIS1-IDDISTR              
080300                                                                          
080400         CALL W460DIS1 USING DIS1-W460DIS1                                
080500                                                                          
081100         IF GMT-FLNC = JA OR DIS1-KDSVAR = JA                             
081200           OR DIS130-NOAC                                                 
081210                                                                          
081232             IF W-OLD-VIPS-IDDISTR = SORTWS-IDDISTR                       
081233               CONTINUE                                                   
081234             ELSE                                                         
081235               IF DIS1-IDLANDX2 = W-SLUTPOST-IDLANDX2                     
081236                                                                          
081237***              OLIKA DISTRIKT INOM SAMMA LAND SKA ÄNDÅ                  
081238***              TILL SAMMA FIL                                           
081239                 CONTINUE                                                 
081240               ELSE                                                       
081250                 IF W-OLD-VIPS-IDDISTR > ZERO                             
081260                   PERFORM CC-BEHANDLA-SLUTPOST                           
081270                 END-IF                                                   
081271                 MOVE SORTWS-IDDISTR TO W-OLD-VIPS-IDDISTR                
081272                 MOVE +0           TO  W-ANT-POSTER                       
081273                 MOVE DIS1-IDLANDX2 TO W-SLUTPOST-IDLANDX2                
081280                 PERFORM CB-BEHANDLA-STARTPOST                            
081281               END-IF                                                     
081290             END-IF                                                       
081291                                                                          
081300             PERFORM CA-BEHANDLA-POST-PER-DISTR                           
081600         END-IF                                                           
081700         PERFORM S32-SORT-RETURN                                          
081800     END-PERFORM                                                          
081801                                                                          
081802*    SISTA SLUTPOSTEN EFTER EOF SKA SKRIVAS                               
081810     IF W-OLD-VIPS-IDDISTR > ZERO                                         
081820       PERFORM CC-BEHANDLA-SLUTPOST                                       
081830     END-IF                                                               
081900     .                                                                    
082000     EJECT                                                                
082100 CA-BEHANDLA-POST-PER-DISTR SECTION.                                      
082500                                                                          
082600     EVALUATE DIS1-IDLANDX2                                               
082700                                                                          
082800        WHEN  ISO-SVERIGE                                                 
083310                  PERFORM CAA-SKAPA-POSTER-ALT1                           
083320                  PERFORM CAF-SKAPA-RILN-POST                             
083330                  WRITE UTPOST-SE FROM RIK1-RIK-W461RIK1                  
083340                  WRITE UTPOST-SE FROM RILN-RIL-W461RILN-CTX              
083350                  WRITE UTPOST-SE FROM RIM2-RIM-W461RIM2-CTX              
083360                  WRITE UTPOST-SE FROM RIN0-RIN-W461RIN0-CTX              
083370                  WRITE UTPOST-SE FROM RIO2-RIO-W461RIO2                  
083380                  ADD +5 TO W-ANT-POSTER                                  
083400                                                                          
083500        WHEN  ISO-NORGE                                                   
084010                  PERFORM CAA-SKAPA-POSTER-ALT1                           
084020                  PERFORM CAF-SKAPA-RILN-POST                             
084030                  WRITE UTPOST-NO FROM RIK1-RIK-W461RIK1                  
084040                  WRITE UTPOST-NO FROM RILN-RIL-W461RILN-CTX              
084050                  WRITE UTPOST-NO FROM RIM2-RIM-W461RIM2-CTX              
084060                  WRITE UTPOST-NO FROM RIN0-RIN-W461RIN0-CTX              
084070                  WRITE UTPOST-NO FROM RIO2-RIO-W461RIO2                  
084080                  ADD +5 TO W-ANT-POSTER                                  
084100                                                                          
084200        WHEN  ISO-DANMARK                                                 
084710                  PERFORM CAA-SKAPA-POSTER-ALT1                           
084720                  PERFORM CAF-SKAPA-RILN-POST                             
084730                  WRITE UTPOST-DK FROM RIK1-RIK-W461RIK1                  
084740                  WRITE UTPOST-DK FROM RILN-RIL-W461RILN-CTX              
084750                  WRITE UTPOST-DK FROM RIM2-RIM-W461RIM2-CTX              
084760                  WRITE UTPOST-DK FROM RIN0-RIN-W461RIN0-CTX              
084770                  WRITE UTPOST-DK FROM RIO2-RIO-W461RIO2                  
084780                  ADD +5 TO W-ANT-POSTER                                  
084800                                                                          
084900        WHEN  ISO-FINLAND                                                 
085410                  PERFORM CAA-SKAPA-POSTER-ALT1                           
085420                  PERFORM CAF-SKAPA-RILN-POST                             
085430                  WRITE UTPOST-FI FROM RIK1-RIK-W461RIK1                  
085440                  WRITE UTPOST-FI FROM RILN-RIL-W461RILN-CTX              
085450                  WRITE UTPOST-FI FROM RIM2-RIM-W461RIM2-CTX              
085460                  WRITE UTPOST-FI FROM RIN0-RIN-W461RIN0-CTX              
085470                  WRITE UTPOST-FI FROM RIO2-RIO-W461RIO2                  
085480                  ADD +5 TO W-ANT-POSTER                                  
085500                                                                          
085600        WHEN  ISO-BELGIEN                                                 
086110                  PERFORM CAA-SKAPA-POSTER-ALT1                           
086120                  PERFORM CAF-SKAPA-RILN-POST                             
086130                  WRITE UTPOST-BE FROM RIK1-RIK-W461RIK1                  
086140                  WRITE UTPOST-BE FROM RILN-RIL-W461RILN-CTX              
086150                  WRITE UTPOST-BE FROM RIM2-RIM-W461RIM2-CTX              
086160                  WRITE UTPOST-BE FROM RIN0-RIN-W461RIN0-CTX              
086170                  WRITE UTPOST-BE FROM RIO2-RIO-W461RIO2                  
086180                  ADD +5 TO W-ANT-POSTER                                  
086200                                                                          
086300        WHEN  ISO-ENGLAND                                                 
086810                  PERFORM CAA-SKAPA-POSTER-ALT1                           
086820                  PERFORM CAF-SKAPA-RILN-POST                             
086830                  WRITE UTPOST-GB FROM RIK1-RIK-W461RIK1                  
086840                  WRITE UTPOST-GB FROM RILN-RIL-W461RILN-CTX              
086850                  WRITE UTPOST-GB FROM RIM2-RIM-W461RIM2-CTX              
086860                  WRITE UTPOST-GB FROM RIN0-RIN-W461RIN0-CTX              
086870                  WRITE UTPOST-GB FROM RIO2-RIO-W461RIO2                  
086880                  ADD +5 TO W-ANT-POSTER                                  
086900                                                                          
087000        WHEN  ISO-FRANKRIKE                                               
087510                  PERFORM CAA-SKAPA-POSTER-ALT1                           
087520                  PERFORM CAF-SKAPA-RILN-POST                             
087530                  WRITE UTPOST-FR FROM RIK1-RIK-W461RIK1                  
087540                  WRITE UTPOST-FR FROM RILN-RIL-W461RILN-CTX              
087550                  WRITE UTPOST-FR FROM RIM2-RIM-W461RIM2-CTX              
087560                  WRITE UTPOST-FR FROM RIN0-RIN-W461RIN0-CTX              
087570                  WRITE UTPOST-FR FROM RIO2-RIO-W461RIO2                  
087580                  ADD +5 TO W-ANT-POSTER                                  
087600                                                                          
087700        WHEN  ISO-HOLLAND                                                 
087800                  PERFORM CAA-SKAPA-POSTER-ALT1                           
087810                  PERFORM CAF-SKAPA-RILN-POST                             
087900                  WRITE UTPOST-NL FROM RIK1-RIK-W461RIK1                  
087910                  WRITE UTPOST-NL FROM RILN-RIL-W461RILN-CTX              
088000                  WRITE UTPOST-NL FROM RIM2-RIM-W461RIM2-CTX              
088100                  WRITE UTPOST-NL FROM RIN0-RIN-W461RIN0-CTX              
088200                  WRITE UTPOST-NL FROM RIO2-RIO-W461RIO2                  
088210                  ADD +5 TO W-ANT-POSTER                                  
088300                                                                          
088400        WHEN  ISO-IRLAND                                                  
088910                  PERFORM CAA-SKAPA-POSTER-ALT1                           
088920                  PERFORM CAF-SKAPA-RILN-POST                             
088930                  WRITE UTPOST-EI FROM RIK1-RIK-W461RIK1                  
088940                  WRITE UTPOST-EI FROM RILN-RIL-W461RILN-CTX              
088950                  WRITE UTPOST-EI FROM RIM2-RIM-W461RIM2-CTX              
088960                  WRITE UTPOST-EI FROM RIN0-RIN-W461RIN0-CTX              
088970                  WRITE UTPOST-EI FROM RIO2-RIO-W461RIO2                  
088980                  ADD +5 TO W-ANT-POSTER                                  
089000                                                                          
089100        WHEN  ISO-ITALIEN                                                 
089610                  PERFORM CAA-SKAPA-POSTER-ALT1                           
089620                  PERFORM CAF-SKAPA-RILN-POST                             
089630                  WRITE UTPOST-IT FROM RIK1-RIK-W461RIK1                  
089640                  WRITE UTPOST-IT FROM RILN-RIL-W461RILN-CTX              
089650                  WRITE UTPOST-IT FROM RIM2-RIM-W461RIM2-CTX              
089660                  WRITE UTPOST-IT FROM RIN0-RIN-W461RIN0-CTX              
089670                  WRITE UTPOST-IT FROM RIO2-RIO-W461RIO2                  
089680                  ADD +5 TO W-ANT-POSTER                                  
089700                                                                          
089800        WHEN  ISO-PORTUGAL                                                
090310                  PERFORM CAA-SKAPA-POSTER-ALT1                           
090320                  PERFORM CAF-SKAPA-RILN-POST                             
090330                  WRITE UTPOST-PT FROM RIK1-RIK-W461RIK1                  
090340                  WRITE UTPOST-PT FROM RILN-RIL-W461RILN-CTX              
090350                  WRITE UTPOST-PT FROM RIM2-RIM-W461RIM2-CTX              
090360                  WRITE UTPOST-PT FROM RIN0-RIN-W461RIN0-CTX              
090370                  WRITE UTPOST-PT FROM RIO2-RIO-W461RIO2                  
090380                  ADD +5 TO W-ANT-POSTER                                  
090400                                                                          
090500        WHEN  ISO-PORTUGAL2                                               
091010                  PERFORM CAA-SKAPA-POSTER-ALT1                           
091020                  PERFORM CAF-SKAPA-RILN-POST                             
091030                  WRITE UTPOST-PX FROM RIK1-RIK-W461RIK1                  
091040                  WRITE UTPOST-PX FROM RILN-RIL-W461RILN-CTX              
091050                  WRITE UTPOST-PX FROM RIM2-RIM-W461RIM2-CTX              
091060                  WRITE UTPOST-PX FROM RIN0-RIN-W461RIN0-CTX              
091070                  WRITE UTPOST-PX FROM RIO2-RIO-W461RIO2                  
091080                  ADD +5 TO W-ANT-POSTER                                  
091100                                                                          
091200        WHEN  ISO-SCHWEIZ                                                 
091710                  PERFORM CAA-SKAPA-POSTER-ALT1                           
091720                  PERFORM CAF-SKAPA-RILN-POST                             
091730                  WRITE UTPOST-CH FROM RIK1-RIK-W461RIK1                  
091740                  WRITE UTPOST-CH FROM RILN-RIL-W461RILN-CTX              
091750                  WRITE UTPOST-CH FROM RIM2-RIM-W461RIM2-CTX              
091760                  WRITE UTPOST-CH FROM RIN0-RIN-W461RIN0-CTX              
091770                  WRITE UTPOST-CH FROM RIO2-RIO-W461RIO2                  
091780                  ADD +5 TO W-ANT-POSTER                                  
091800                                                                          
091900        WHEN  ISO-SPANIEN                                                 
092410                  PERFORM CAA-SKAPA-POSTER-ALT1                           
092420                  PERFORM CAF-SKAPA-RILN-POST                             
092430                  WRITE UTPOST-ES FROM RIK1-RIK-W461RIK1                  
092440                  WRITE UTPOST-ES FROM RILN-RIL-W461RILN-CTX              
092450                  WRITE UTPOST-ES FROM RIM2-RIM-W461RIM2-CTX              
092460                  WRITE UTPOST-ES FROM RIN0-RIN-W461RIN0-CTX              
092470                  WRITE UTPOST-ES FROM RIO2-RIO-W461RIO2                  
092480                  ADD +5 TO W-ANT-POSTER                                  
092500                                                                          
092600        WHEN  ISO-TYSKLAND                                                
093110                  PERFORM CAA-SKAPA-POSTER-ALT1                           
093120                  PERFORM CAF-SKAPA-RILN-POST                             
093130                  WRITE UTPOST-DE FROM RIK1-RIK-W461RIK1                  
093140                  WRITE UTPOST-DE FROM RILN-RIL-W461RILN-CTX              
093150                  WRITE UTPOST-DE FROM RIM2-RIM-W461RIM2-CTX              
093160                  WRITE UTPOST-DE FROM RIN0-RIN-W461RIN0-CTX              
093170                  WRITE UTPOST-DE FROM RIO2-RIO-W461RIO2                  
093180                  ADD +5 TO W-ANT-POSTER                                  
093200                                                                          
093300        WHEN  ISO-OSTERRIKE                                               
093810                  PERFORM CAA-SKAPA-POSTER-ALT1                           
093820                  PERFORM CAF-SKAPA-RILN-POST                             
093830                  WRITE UTPOST-AT FROM RIK1-RIK-W461RIK1                  
093840                  WRITE UTPOST-AT FROM RILN-RIL-W461RILN-CTX              
093850                  WRITE UTPOST-AT FROM RIM2-RIM-W461RIM2-CTX              
093860                  WRITE UTPOST-AT FROM RIN0-RIN-W461RIN0-CTX              
093870                  WRITE UTPOST-AT FROM RIO2-RIO-W461RIO2                  
093880                  ADD +5 TO W-ANT-POSTER                                  
093900                                                                          
094000        WHEN  ISO-POLEN                                                   
094300                  PERFORM CAA-SKAPA-POSTER-ALT1                           
094400                  PERFORM CAF-SKAPA-RILN-POST                             
094500                  WRITE UTPOST-PL FROM RIK1-RIK-W461RIK1                  
094600                  WRITE UTPOST-PL FROM RILN-RIL-W461RILN-CTX              
094700                  WRITE UTPOST-PL FROM RIM2-RIM-W461RIM2-CTX              
094800                  WRITE UTPOST-PL FROM RIN0-RIN-W461RIN0-CTX              
094900                  WRITE UTPOST-PL FROM RIO2-RIO-W461RIO2                  
095000                  ADD +5 TO W-ANT-POSTER                                  
095300                                                                          
095400        WHEN  ISO-SAUDI                                                   
095500                  PERFORM CAC-SKAPA-POSTER-ALT3                           
095510                  PERFORM CAF-SKAPA-RILN-POST                             
095600                  WRITE UTPOST-SA FROM RIK0-RIK-W461RIK0                  
095610                  WRITE UTPOST-SA FROM RILN-RIL-W461RILN-CTX              
095700                  WRITE UTPOST-SA FROM RIM0-RIM-W461RIM0-CTX              
095800                  WRITE UTPOST-SA FROM RIN0-RIN-W461RIN0-CTX              
095900                  WRITE UTPOST-SA FROM RIO2-RIO-W461RIO2                  
095910                  ADD +5 TO W-ANT-POSTER                                  
096000                                                                          
096100        WHEN  ISO-MALAYSIA                                                
096610                  PERFORM CAA-SKAPA-POSTER-ALT1                           
096620                  PERFORM CAF-SKAPA-RILN-POST                             
096630                  WRITE UTPOST-MY FROM RIK1-RIK-W461RIK1                  
096640                  WRITE UTPOST-MY FROM RILN-RIL-W461RILN-CTX              
096650                  WRITE UTPOST-MY FROM RIM2-RIM-W461RIM2-CTX              
096660                  WRITE UTPOST-MY FROM RIN0-RIN-W461RIN0-CTX              
096670                  WRITE UTPOST-MY FROM RIO2-RIO-W461RIO2                  
096680                  ADD +5 TO W-ANT-POSTER                                  
096700                                                                          
096800        WHEN  ISO-TURKIET                                                 
097310                  PERFORM CAA-SKAPA-POSTER-ALT1                           
097320                  PERFORM CAF-SKAPA-RILN-POST                             
097330                  WRITE UTPOST-TR FROM RIK1-RIK-W461RIK1                  
097340                  WRITE UTPOST-TR FROM RILN-RIL-W461RILN-CTX              
097350                  WRITE UTPOST-TR FROM RIM2-RIM-W461RIM2-CTX              
097360                  WRITE UTPOST-TR FROM RIN0-RIN-W461RIN0-CTX              
097370                  WRITE UTPOST-TR FROM RIO2-RIO-W461RIO2                  
097380                  ADD +5 TO W-ANT-POSTER                                  
097400                                                                          
097500        WHEN  ISO-KOREA                                                   
098010                  PERFORM CAA-SKAPA-POSTER-ALT1                           
098020                  PERFORM CAF-SKAPA-RILN-POST                             
098030                  WRITE UTPOST-KR FROM RIK1-RIK-W461RIK1                  
098040                  WRITE UTPOST-KR FROM RILN-RIL-W461RILN-CTX              
098050                  WRITE UTPOST-KR FROM RIM2-RIM-W461RIM2-CTX              
098060                  WRITE UTPOST-KR FROM RIN0-RIN-W461RIN0-CTX              
098070                  WRITE UTPOST-KR FROM RIO2-RIO-W461RIO2                  
098080                  ADD +5 TO W-ANT-POSTER                                  
098100                                                                          
098200        WHEN  ISO-TAIWAN                                                  
098710                  PERFORM CAA-SKAPA-POSTER-ALT1                           
098720                  PERFORM CAF-SKAPA-RILN-POST                             
098730                  WRITE UTPOST-TX FROM RIK1-RIK-W461RIK1                  
098740                  WRITE UTPOST-TX FROM RILN-RIL-W461RILN-CTX              
098750                  WRITE UTPOST-TX FROM RIM2-RIM-W461RIM2-CTX              
098760                  WRITE UTPOST-TX FROM RIN0-RIN-W461RIN0-CTX              
098770                  WRITE UTPOST-TX FROM RIO2-RIO-W461RIO2                  
098780                  ADD +5 TO W-ANT-POSTER                                  
098800                                                                          
098900        WHEN  ISO-TAIWAN2                                                 
099410                  PERFORM CAA-SKAPA-POSTER-ALT1                           
099420                  PERFORM CAF-SKAPA-RILN-POST                             
099430                  WRITE UTPOST-TW FROM RIK1-RIK-W461RIK1                  
099440                  WRITE UTPOST-TW FROM RILN-RIL-W461RILN-CTX              
099450                  WRITE UTPOST-TW FROM RIM2-RIM-W461RIM2-CTX              
099460                  WRITE UTPOST-TW FROM RIN0-RIN-W461RIN0-CTX              
099470                  WRITE UTPOST-TW FROM RIO2-RIO-W461RIO2                  
099480                  ADD +5 TO W-ANT-POSTER                                  
099500                                                                          
099600        WHEN  ISO-MEXICO                                                  
100110                  PERFORM CAA-SKAPA-POSTER-ALT1                           
100120                  PERFORM CAF-SKAPA-RILN-POST                             
100130                  WRITE UTPOST-MX FROM RIK1-RIK-W461RIK1                  
100140                  WRITE UTPOST-MX FROM RILN-RIL-W461RILN-CTX              
100150                  WRITE UTPOST-MX FROM RIM2-RIM-W461RIM2-CTX              
100160                  WRITE UTPOST-MX FROM RIN0-RIN-W461RIN0-CTX              
100170                  WRITE UTPOST-MX FROM RIO2-RIO-W461RIO2                  
100180                  ADD +5 TO W-ANT-POSTER                                  
100200                                                                          
100300        WHEN  ISO-BRASILIEN                                               
100810                  PERFORM CAA-SKAPA-POSTER-ALT1                           
100820                  PERFORM CAF-SKAPA-RILN-POST                             
100830                  WRITE UTPOST-BR FROM RIK1-RIK-W461RIK1                  
100840                  WRITE UTPOST-BR FROM RILN-RIL-W461RILN-CTX              
100850                  WRITE UTPOST-BR FROM RIM2-RIM-W461RIM2-CTX              
100860                  WRITE UTPOST-BR FROM RIN0-RIN-W461RIN0-CTX              
100870                  WRITE UTPOST-BR FROM RIO2-RIO-W461RIO2                  
100880                  ADD +5 TO W-ANT-POSTER                                  
100900                                                                          
101010**NEDANSTÅENDE DISTR HAR EJ HITTATS I ANALYS                              
101100**SÅLEDES ATT DET ÄR RÄTT COPIES VALDA ÄR EJ BEKRÄFTADE                   
101200                                                                          
101300        WHEN  ISO-CANADA                                                  
101810                  PERFORM CAA-SKAPA-POSTER-ALT1                           
101820                  PERFORM CAF-SKAPA-RILN-POST                             
101830                  WRITE UTPOST-CA FROM RIK1-RIK-W461RIK1                  
101840                  WRITE UTPOST-CA FROM RILN-RIL-W461RILN-CTX              
101850                  WRITE UTPOST-CA FROM RIM2-RIM-W461RIM2-CTX              
101860                  WRITE UTPOST-CA FROM RIN0-RIN-W461RIN0-CTX              
101870                  WRITE UTPOST-CA FROM RIO2-RIO-W461RIO2                  
101880                  ADD +5 TO W-ANT-POSTER                                  
101900                                                                          
102000        WHEN  ISO-JAPAN                                                   
102510                  PERFORM CAA-SKAPA-POSTER-ALT1                           
102520                  PERFORM CAF-SKAPA-RILN-POST                             
102530                  WRITE UTPOST-JP FROM RIK1-RIK-W461RIK1                  
102540                  WRITE UTPOST-JP FROM RILN-RIL-W461RILN-CTX              
102550                  WRITE UTPOST-JP FROM RIM2-RIM-W461RIM2-CTX              
102560                  WRITE UTPOST-JP FROM RIN0-RIN-W461RIN0-CTX              
102570                  WRITE UTPOST-JP FROM RIO2-RIO-W461RIO2                  
102580                  ADD +5 TO W-ANT-POSTER                                  
102600                                                                          
103400        WHEN  ISO-KINA-C1                                                 
103910                  PERFORM CAA-SKAPA-POSTER-ALT1                           
103920                  PERFORM CAF-SKAPA-RILN-POST                             
103930                  WRITE UTPOST-C1 FROM RIK1-RIK-W461RIK1                  
103940                  WRITE UTPOST-C1 FROM RILN-RIL-W461RILN-CTX              
103950                  WRITE UTPOST-C1 FROM RIM2-RIM-W461RIM2-CTX              
103960                  WRITE UTPOST-C1 FROM RIN0-RIN-W461RIN0-CTX              
103970                  WRITE UTPOST-C1 FROM RIO2-RIO-W461RIO2                  
103980                  ADD +5 TO W-ANT-POSTER                                  
104000                                                                          
104100        WHEN  ISO-PERU                                                    
104610                  PERFORM CAA-SKAPA-POSTER-ALT1                           
104620                  PERFORM CAF-SKAPA-RILN-POST                             
104630                  WRITE UTPOST-PE FROM RIK1-RIK-W461RIK1                  
104640                  WRITE UTPOST-PE FROM RILN-RIL-W461RILN-CTX              
104650                  WRITE UTPOST-PE FROM RIM2-RIM-W461RIM2-CTX              
104660                  WRITE UTPOST-PE FROM RIN0-RIN-W461RIN0-CTX              
104670                  WRITE UTPOST-PE FROM RIO2-RIO-W461RIO2                  
104680                  ADD +5 TO W-ANT-POSTER                                  
104700                                                                          
104800        WHEN  ISO-RYSSLAND                                                
105310                  PERFORM CAA-SKAPA-POSTER-ALT1                           
105320                  PERFORM CAF-SKAPA-RILN-POST                             
105330                  WRITE UTPOST-RU FROM RIK1-RIK-W461RIK1                  
105340                  WRITE UTPOST-RU FROM RILN-RIL-W461RILN-CTX              
105350                  WRITE UTPOST-RU FROM RIM2-RIM-W461RIM2-CTX              
105360                  WRITE UTPOST-RU FROM RIN0-RIN-W461RIN0-CTX              
105370                  WRITE UTPOST-RU FROM RIO2-RIO-W461RIO2                  
105380                  ADD +5 TO W-ANT-POSTER                                  
105400                                                                          
105500        WHEN  ISO-SYDAFRIKA                                               
106010                  PERFORM CAA-SKAPA-POSTER-ALT1                           
106020                  PERFORM CAF-SKAPA-RILN-POST                             
106030                  WRITE UTPOST-ZA FROM RIK1-RIK-W461RIK1                  
106040                  WRITE UTPOST-ZA FROM RILN-RIL-W461RILN-CTX              
106050                  WRITE UTPOST-ZA FROM RIM2-RIM-W461RIM2-CTX              
106060                  WRITE UTPOST-ZA FROM RIN0-RIN-W461RIN0-CTX              
106070                  WRITE UTPOST-ZA FROM RIO2-RIO-W461RIO2                  
106080                  ADD +5 TO W-ANT-POSTER                                  
106100                                                                          
106200        WHEN  ISO-USA                                                     
106710                  PERFORM CAA-SKAPA-POSTER-ALT1                           
106720                  PERFORM CAF-SKAPA-RILN-POST                             
106730                  WRITE UTPOST-US FROM RIK1-RIK-W461RIK1                  
106740                  WRITE UTPOST-US FROM RILN-RIL-W461RILN-CTX              
106750                  WRITE UTPOST-US FROM RIM2-RIM-W461RIM2-CTX              
106760                  WRITE UTPOST-US FROM RIN0-RIN-W461RIN0-CTX              
106770                  WRITE UTPOST-US FROM RIO2-RIO-W461RIO2                  
106780                  ADD +5 TO W-ANT-POSTER                                  
106810                                                                          
106811        WHEN  ISO-TJECKIEN                                                
106820                  PERFORM CAA-SKAPA-POSTER-ALT1                           
106830                  PERFORM CAF-SKAPA-RILN-POST                             
106840                  WRITE UTPOST-CZ FROM RIK1-RIK-W461RIK1                  
106850                  WRITE UTPOST-CZ FROM RILN-RIL-W461RILN-CTX              
106860                  WRITE UTPOST-CZ FROM RIM2-RIM-W461RIM2-CTX              
106870                  WRITE UTPOST-CZ FROM RIN0-RIN-W461RIN0-CTX              
106880                  WRITE UTPOST-CZ FROM RIO2-RIO-W461RIO2                  
106890                  ADD +5 TO W-ANT-POSTER                                  
106891                                                                          
106892        WHEN  ISO-UNGERN                                                  
106893                  PERFORM CAA-SKAPA-POSTER-ALT1                           
106894                  PERFORM CAF-SKAPA-RILN-POST                             
106895                  WRITE UTPOST-HU FROM RIK1-RIK-W461RIK1                  
106896                  WRITE UTPOST-HU FROM RILN-RIL-W461RILN-CTX              
106897                  WRITE UTPOST-HU FROM RIM2-RIM-W461RIM2-CTX              
106898                  WRITE UTPOST-HU FROM RIN0-RIN-W461RIN0-CTX              
106899                  WRITE UTPOST-HU FROM RIO2-RIO-W461RIO2                  
106900                  ADD +5 TO W-ANT-POSTER                                  
106901                                                                          
106902        WHEN  ISO-INDIEN                                                  
106903                  PERFORM CAA-SKAPA-POSTER-ALT1                           
106904                  PERFORM CAF-SKAPA-RILN-POST                             
106905                  WRITE UTPOST-IN FROM RIK1-RIK-W461RIK1                  
106906                  WRITE UTPOST-IN FROM RILN-RIL-W461RILN-CTX              
106907                  WRITE UTPOST-IN FROM RIM2-RIM-W461RIM2-CTX              
106908                  WRITE UTPOST-IN FROM RIN0-RIN-W461RIN0-CTX              
106909                  WRITE UTPOST-IN FROM RIO2-RIO-W461RIO2                  
106910                  ADD +5 TO W-ANT-POSTER                                  
106911                                                                          
106912        WHEN  ISO-THAILAND                                                
106913                  PERFORM CAA-SKAPA-POSTER-ALT1                           
106914                  PERFORM CAF-SKAPA-RILN-POST                             
106915                  WRITE UTPOST-TH FROM RIK1-RIK-W461RIK1                  
106916                  WRITE UTPOST-TH FROM RILN-RIL-W461RILN-CTX              
106917                  WRITE UTPOST-TH FROM RIM2-RIM-W461RIM2-CTX              
106918                  WRITE UTPOST-TH FROM RIN0-RIN-W461RIN0-CTX              
106919                  WRITE UTPOST-TH FROM RIO2-RIO-W461RIO2                  
106920                  ADD +5 TO W-ANT-POSTER                                  
106921                                                                          
106922        WHEN  ISO-AUSTRALIEN                                              
106923                  PERFORM CAA-SKAPA-POSTER-ALT1                           
106924                  PERFORM CAF-SKAPA-RILN-POST                             
106925                  WRITE UTPOST-AU FROM RIK1-RIK-W461RIK1                  
106926                  WRITE UTPOST-AU FROM RILN-RIL-W461RILN-CTX              
106927                  WRITE UTPOST-AU FROM RIM2-RIM-W461RIM2-CTX              
106928                  WRITE UTPOST-AU FROM RIN0-RIN-W461RIN0-CTX              
106929                  WRITE UTPOST-AU FROM RIO2-RIO-W461RIO2                  
106930                  ADD +5 TO W-ANT-POSTER                                  
106931                                                                          
106940        WHEN  OTHER                                                       
107000                        DISPLAY '*****  OBS!! EJ UTVALT IDDISTR:'         
107100                                 SORTWS-IDDISTR   '******'                
107200     END-EVALUATE                                                         
107300                                                                          
107400     .                                                                    
107500     EJECT                                                                
107600 CAA-SKAPA-POSTER-ALT1      SECTION.                                      
107700                                                                          
107800*    SKAPA POSTER FÖR FLERTALET DISTRIKT/LÄNDER                           
107900                                                                          
108000     PERFORM CAAA-SKAPA-RIK1-POST                                         
108100     PERFORM CAAB-SKAPA-RIM2-POST                                         
108200     PERFORM S30A-SKAPA-RIN0-POST                                         
108300     PERFORM S30B-SKAPA-RIO2-POST                                         
108400     .                                                                    
108500 CAAA-SKAPA-RIK1-POST      SECTION.                                       
108600                                                                          
108700     INITIALIZE RIK1-RIK-W461RIK1                                         
108800     MOVE 'RIK'          TO RIK1-RIK-IDPTYP                               
108900     MOVE W-IDDC-11      TO RIK1-RIK-IDDC                                 
109000     MOVE SORT-IDDISTR   TO RIK1-RIK-IDDISTR                              
109100     MOVE W-KDFAKTYP-S   TO RIK1-RIK-KDFAKTYP                             
109200     MOVE W-PRKURS-1     TO RIK1-RIK-PRKURS                               
109300     .                                                                    
109400 CAAB-SKAPA-RIM2-POST      SECTION.                                       
109500                                                                          
109600     INITIALIZE RIM2-RIM-W461RIM2-CTX                                     
109700     MOVE 'RIM'          TO RIM2-RIM-IDPTYP                               
109800     MOVE SORT-IDKUNDNR  TO RIM2-RIM-IDKUNDNR                             
109900     MOVE SORT-IDORDNR7  TO RIM2-RIM-IDORDNR                              
110000     MOVE SORT-IDPRODNR  TO RIM2-RIM-IDPRODNR                             
110010     MOVE W-KDFRAKT-88   TO RIM2-RIM-KDFRAKT                              
110100     MOVE SORT-IDVIN     TO RIM2-RIM-IDVIN                                
110200     .                                                                    
116200 CAC-SKAPA-POSTER-ALT3      SECTION.                                      
116300                                                                          
116400*    SKAPA POSTER SA,SAUDI                                                
116500                                                                          
116600     PERFORM CACA-SKAPA-RIK0-POST                                         
116700     PERFORM CACB-SKAPA-RIM0-POST                                         
116800     PERFORM S30A-SKAPA-RIN0-POST                                         
116900     PERFORM S30B-SKAPA-RIO2-POST                                         
117000     .                                                                    
117100 CACA-SKAPA-RIK0-POST      SECTION.                                       
117200                                                                          
117300     INITIALIZE RIK0-RIK-W461RIK0                                         
117400     MOVE 'RIK'          TO RIK0-RIK-IDPTYP                               
117500     MOVE W-KDCLAGER-1   TO RIK0-RIK-KDCLAGER                             
117600     MOVE SORT-IDDISTR   TO RIK0-RIK-IDDISTR                              
117700     MOVE W-KDFAKTYP-S   TO RIK0-RIK-KDFAKTYP                             
117800     MOVE W-PRKURS-1     TO RIK0-RIK-PRKURS                               
117900     .                                                                    
118000 CACB-SKAPA-RIM0-POST      SECTION.                                       
118100                                                                          
118200     INITIALIZE RIM0-RIM-W461RIM0-CTX                                     
118300     MOVE 'RIM'          TO RIM0-RIM-IDPTYP                               
118400     MOVE SORT-IDKUNDNR  TO RIM0-RIM-IDKUNDNR                             
118500     MOVE SORT-IDORDNR7  TO RIM0-RIM-IDORDNR                              
118600     MOVE SORT-IDPRODNR  TO RIM0-RIM-IDPRODNR                             
118610     MOVE W-KDFRAKT-88   TO RIM0-RIM-KDFRAKT                              
118700     .                                                                    
120300 CAF-SKAPA-RILN-POST      SECTION.                                        
120400                                                                          
120500     INITIALIZE RILN-RIL-W461RILN-CTX                                     
120600     MOVE 'RIL'          TO RILN-RIL-IDPTYP                               
120700     MOVE W-KDFRAKT-88   TO RILN-RIL-KDFRAKT                              
120800     .                                                                    
120810 CB-BEHANDLA-STARTPOST    SECTION.                                        
120811                                                                          
120812*    SKRIV STARTPOST TILL RESP MARKNADS FIL                               
120813                                                                          
120814     INITIALIZE RI0N-START-W461RI0N-CTX                                   
120815     MOVE 'RI0'          TO RI0N-START-IDPTYP                             
120816     MOVE SORT-IDDISTR   TO RI0N-START-IDDISTR                            
120817     MOVE DAGENS-DATUM   TO RI0N-START-TIFILDAT                           
120818     MOVE DAGENS-TID     TO RI0N-START-TIHHMMSS                           
120819     MOVE W-IDDC-11      TO RI0N-START-IDDC                               
120820                                                                          
120828     EVALUATE DIS1-IDLANDX2                                               
120829                                                                          
120830        WHEN  ISO-SVERIGE                                                 
120831              WRITE UTPOST-SE FROM RI0N-START-W461RI0N-CTX                
120832                                                                          
120833        WHEN  ISO-NORGE                                                   
120837              WRITE UTPOST-NO FROM RI0N-START-W461RI0N-CTX                
120838                                                                          
120839        WHEN  ISO-DANMARK                                                 
120844              WRITE UTPOST-DK FROM RI0N-START-W461RI0N-CTX                
120845                                                                          
120846        WHEN  ISO-FINLAND                                                 
120851              WRITE UTPOST-FI FROM RI0N-START-W461RI0N-CTX                
120852                                                                          
120853        WHEN  ISO-BELGIEN                                                 
120858              WRITE UTPOST-BE FROM RI0N-START-W461RI0N-CTX                
120859                                                                          
120860        WHEN  ISO-ENGLAND                                                 
120865              WRITE UTPOST-GB FROM RI0N-START-W461RI0N-CTX                
120866                                                                          
120867        WHEN  ISO-FRANKRIKE                                               
120872              WRITE UTPOST-FR FROM RI0N-START-W461RI0N-CTX                
120873                                                                          
120874        WHEN  ISO-HOLLAND                                                 
120875              INITIALIZE RI0-START-W461RI0                                
120876              MOVE 'RI0' TO RI0-START-IDPTYP                              
120877              MOVE SORT-IDDISTR TO RI0-START-IDDISTR                      
120878              MOVE DAGENS-DATUM TO RI0-START-TIFILDAT                     
120879              MOVE DAGENS-TID TO RI0-START-TIHHMMSS                       
120880              MOVE W-KDCLAGER-1 TO RI0-START-KDCLAGER                     
120881                                                                          
120882              WRITE UTPOST-NL FROM RI0-START-W461RI0                      
120883                                                                          
120884        WHEN  ISO-IRLAND                                                  
120886              WRITE UTPOST-EI FROM RI0N-START-W461RI0N-CTX                
120887                                                                          
120888        WHEN  ISO-ITALIEN                                                 
120893              WRITE UTPOST-IT FROM RI0N-START-W461RI0N-CTX                
120894                                                                          
120895        WHEN  ISO-PORTUGAL                                                
120900              WRITE UTPOST-PT FROM RI0N-START-W461RI0N-CTX                
120901                                                                          
120902        WHEN  ISO-PORTUGAL2                                               
120907              WRITE UTPOST-PX FROM RI0N-START-W461RI0N-CTX                
120908                                                                          
120909        WHEN  ISO-SCHWEIZ                                                 
120914              WRITE UTPOST-CH FROM RI0N-START-W461RI0N-CTX                
120915                                                                          
120916        WHEN  ISO-SPANIEN                                                 
120921              WRITE UTPOST-ES FROM RI0N-START-W461RI0N-CTX                
120922                                                                          
120923        WHEN  ISO-TYSKLAND                                                
120928              WRITE UTPOST-DE FROM RI0N-START-W461RI0N-CTX                
120929                                                                          
120930        WHEN  ISO-OSTERRIKE                                               
120935              WRITE UTPOST-AT FROM RI0N-START-W461RI0N-CTX                
120936                                                                          
120937        WHEN  ISO-POLEN                                                   
120949              WRITE UTPOST-PL FROM RI0N-START-W461RI0N-CTX                
120950                                                                          
120951        WHEN  ISO-SAUDI                                                   
120952              INITIALIZE RI0-START-W461RI0                                
120953              MOVE 'RI0' TO RI0-START-IDPTYP                              
120954              MOVE SORT-IDDISTR TO RI0-START-IDDISTR                      
120955              MOVE DAGENS-DATUM TO RI0-START-TIFILDAT                     
120956              MOVE DAGENS-TID TO RI0-START-TIHHMMSS                       
120957              MOVE W-KDCLAGER-1 TO RI0-START-KDCLAGER                     
120958                                                                          
120959              WRITE UTPOST-SA FROM RI0-START-W461RI0                      
120960                                                                          
120961        WHEN  ISO-MALAYSIA                                                
120963              WRITE UTPOST-MY FROM RI0N-START-W461RI0N-CTX                
120964                                                                          
120965        WHEN  ISO-TURKIET                                                 
120970              WRITE UTPOST-TR FROM RI0N-START-W461RI0N-CTX                
120971                                                                          
120972        WHEN  ISO-KOREA                                                   
120977              WRITE UTPOST-KR FROM RI0N-START-W461RI0N-CTX                
120978                                                                          
120979        WHEN  ISO-TAIWAN                                                  
120984              WRITE UTPOST-TX FROM RI0N-START-W461RI0N-CTX                
120985                                                                          
120986        WHEN  ISO-TAIWAN2                                                 
120991              WRITE UTPOST-TW FROM RI0N-START-W461RI0N-CTX                
120992                                                                          
120993        WHEN  ISO-MEXICO                                                  
120998              WRITE UTPOST-MX FROM RI0N-START-W461RI0N-CTX                
120999                                                                          
121000        WHEN  ISO-BRASILIEN                                               
121005              WRITE UTPOST-BR FROM RI0N-START-W461RI0N-CTX                
121006                                                                          
121007**NEDANSTÅENDE DISTR HAR EJ HITTATS I ANALYS                              
121008**SÅLEDES ATT DET ÄR RÄTT COPIES VALDA ÄR EJ BEKRÄFTADE                   
121009                                                                          
121010        WHEN  ISO-CANADA                                                  
121015              WRITE UTPOST-CA FROM RI0N-START-W461RI0N-CTX                
121016                                                                          
121017        WHEN  ISO-JAPAN                                                   
121022              WRITE UTPOST-JP FROM RI0N-START-W461RI0N-CTX                
121023                                                                          
121031        WHEN  ISO-KINA-C1                                                 
121036              WRITE UTPOST-C1 FROM RI0N-START-W461RI0N-CTX                
121037                                                                          
121038        WHEN  ISO-PERU                                                    
121043              WRITE UTPOST-PE FROM RI0N-START-W461RI0N-CTX                
121044                                                                          
121045        WHEN  ISO-RYSSLAND                                                
121050              WRITE UTPOST-RU FROM RI0N-START-W461RI0N-CTX                
121051                                                                          
121052        WHEN  ISO-SYDAFRIKA                                               
121057              WRITE UTPOST-ZA FROM RI0N-START-W461RI0N-CTX                
121058                                                                          
121059        WHEN  ISO-USA                                                     
121064              WRITE UTPOST-US FROM RI0N-START-W461RI0N-CTX                
121065                                                                          
121066        WHEN  ISO-TJECKIEN                                                
121067              WRITE UTPOST-CZ FROM RI0N-START-W461RI0N-CTX                
121068                                                                          
121069        WHEN  ISO-UNGERN                                                  
121070              WRITE UTPOST-HU FROM RI0N-START-W461RI0N-CTX                
121071                                                                          
121072        WHEN  ISO-INDIEN                                                  
121073              WRITE UTPOST-IN FROM RI0N-START-W461RI0N-CTX                
121074                                                                          
121075        WHEN  ISO-THAILAND                                                
121076              WRITE UTPOST-TH FROM RI0N-START-W461RI0N-CTX                
121077                                                                          
121078        WHEN  ISO-AUSTRALIEN                                              
121079              WRITE UTPOST-AU FROM RI0N-START-W461RI0N-CTX                
121080                                                                          
121081        WHEN  OTHER                                                       
121082                        DISPLAY '*****  OBS!! EJ UTVALT IDDISTR:'         
121083                                 SORTWS-IDDISTR   '******'                
121084     END-EVALUATE                                                         
121085                                                                          
121086     .                                                                    
121087                                                                          
121088 CC-BEHANDLA-SLUTPOST    SECTION.                                         
121089                                                                          
121090*    SKRIV SLUTPOST TILL RESP MARKNADS FIL                                
121091*    ALLA ANVÄNDER SAMMA COPYTEXT                                         
121092                                                                          
121093     INITIALIZE RI9-SLUT-W461RI9                                          
121094     MOVE 'RI9'          TO RI9-SLUT-IDPTYP                               
121095     MOVE W-ANT-POSTER   TO RI9-SLUT-KVTRANS                              
121096                                                                          
121097     EVALUATE W-SLUTPOST-IDLANDX2                                         
121098                                                                          
121099        WHEN  ISO-SVERIGE                                                 
121100              WRITE UTPOST-SE FROM RI9-SLUT-W461RI9                       
121101                                                                          
121102        WHEN  ISO-NORGE                                                   
121103              WRITE UTPOST-NO FROM RI9-SLUT-W461RI9                       
121104                                                                          
121105        WHEN  ISO-DANMARK                                                 
121106              WRITE UTPOST-DK FROM RI9-SLUT-W461RI9                       
121107                                                                          
121108        WHEN  ISO-FINLAND                                                 
121109              WRITE UTPOST-FI FROM RI9-SLUT-W461RI9                       
121110                                                                          
121111        WHEN  ISO-BELGIEN                                                 
121112              WRITE UTPOST-BE FROM RI9-SLUT-W461RI9                       
121113                                                                          
121114        WHEN  ISO-ENGLAND                                                 
121115              WRITE UTPOST-GB FROM RI9-SLUT-W461RI9                       
121116                                                                          
121117        WHEN  ISO-FRANKRIKE                                               
121118              WRITE UTPOST-FR FROM RI9-SLUT-W461RI9                       
121119                                                                          
121120        WHEN  ISO-HOLLAND                                                 
121121              WRITE UTPOST-NL FROM RI9-SLUT-W461RI9                       
121122                                                                          
121123        WHEN  ISO-IRLAND                                                  
121124              WRITE UTPOST-EI FROM RI9-SLUT-W461RI9                       
121125                                                                          
121126        WHEN  ISO-ITALIEN                                                 
121127              WRITE UTPOST-IT FROM RI9-SLUT-W461RI9                       
121128                                                                          
121129        WHEN  ISO-PORTUGAL                                                
121130              WRITE UTPOST-PT FROM RI9-SLUT-W461RI9                       
121131                                                                          
121132        WHEN  ISO-PORTUGAL2                                               
121133              WRITE UTPOST-PX FROM RI9-SLUT-W461RI9                       
121134                                                                          
121135        WHEN  ISO-SCHWEIZ                                                 
121136              WRITE UTPOST-CH FROM RI9-SLUT-W461RI9                       
121137                                                                          
121138        WHEN  ISO-SPANIEN                                                 
121139              WRITE UTPOST-ES FROM RI9-SLUT-W461RI9                       
121140                                                                          
121141        WHEN  ISO-TYSKLAND                                                
121142              WRITE UTPOST-DE FROM RI9-SLUT-W461RI9                       
121143                                                                          
121144        WHEN  ISO-OSTERRIKE                                               
121145              WRITE UTPOST-AT FROM RI9-SLUT-W461RI9                       
121146                                                                          
121147        WHEN  ISO-POLEN                                                   
121148              WRITE UTPOST-PL FROM RI9-SLUT-W461RI9                       
121149                                                                          
121150        WHEN  ISO-SAUDI                                                   
121151              WRITE UTPOST-SA FROM RI9-SLUT-W461RI9                       
121152                                                                          
121153        WHEN  ISO-MALAYSIA                                                
121154              WRITE UTPOST-MY FROM RI9-SLUT-W461RI9                       
121155                                                                          
121156        WHEN  ISO-TURKIET                                                 
121157              WRITE UTPOST-TR FROM RI9-SLUT-W461RI9                       
121158                                                                          
121159        WHEN  ISO-KOREA                                                   
121160              WRITE UTPOST-KR FROM RI9-SLUT-W461RI9                       
121161                                                                          
121162        WHEN  ISO-TAIWAN                                                  
121163              WRITE UTPOST-TX FROM RI9-SLUT-W461RI9                       
121164                                                                          
121165        WHEN  ISO-TAIWAN2                                                 
121166              WRITE UTPOST-TW FROM RI9-SLUT-W461RI9                       
121167                                                                          
121168        WHEN  ISO-MEXICO                                                  
121169              WRITE UTPOST-MX FROM RI9-SLUT-W461RI9                       
121170                                                                          
121171        WHEN  ISO-BRASILIEN                                               
121172              WRITE UTPOST-BR FROM RI9-SLUT-W461RI9                       
121173                                                                          
121174**NEDANSTÅENDE DISTR HAR EJ HITTATS I ANALYS                              
121175**SÅLEDES ATT DET ÄR RÄTT COPIES VALDA ÄR EJ BEKRÄFTADE                   
121176                                                                          
121177        WHEN  ISO-CANADA                                                  
121178              WRITE UTPOST-CA FROM RI9-SLUT-W461RI9                       
121179                                                                          
121180        WHEN  ISO-JAPAN                                                   
121181              WRITE UTPOST-JP FROM RI9-SLUT-W461RI9                       
121182                                                                          
121183        WHEN  ISO-KINA-C1                                                 
121184              WRITE UTPOST-C1 FROM RI9-SLUT-W461RI9                       
121185                                                                          
121186        WHEN  ISO-PERU                                                    
121187              WRITE UTPOST-PE FROM RI9-SLUT-W461RI9                       
121188                                                                          
121189        WHEN  ISO-RYSSLAND                                                
121190              WRITE UTPOST-RU FROM RI9-SLUT-W461RI9                       
121191                                                                          
121192        WHEN  ISO-SYDAFRIKA                                               
121193              WRITE UTPOST-ZA FROM RI9-SLUT-W461RI9                       
121194                                                                          
121195        WHEN  ISO-USA                                                     
121196              WRITE UTPOST-US FROM RI9-SLUT-W461RI9                       
121197                                                                          
121198        WHEN  ISO-TJECKIEN                                                
121199              WRITE UTPOST-CZ FROM RI9-SLUT-W461RI9                       
121200                                                                          
121201        WHEN  ISO-UNGERN                                                  
121202              WRITE UTPOST-HU FROM RI9-SLUT-W461RI9                       
121203                                                                          
121204        WHEN  ISO-INDIEN                                                  
121205              WRITE UTPOST-IN FROM RI9-SLUT-W461RI9                       
121206                                                                          
121207        WHEN  ISO-THAILAND                                                
121208              WRITE UTPOST-TH FROM RI9-SLUT-W461RI9                       
121209                                                                          
121210        WHEN  ISO-AUSTRALIEN                                              
121211              WRITE UTPOST-AU FROM RI9-SLUT-W461RI9                       
121212                                                                          
121213        WHEN  OTHER                                                       
121214                        DISPLAY '*****  OBS!! EJ UTVALT IDDISTR:'         
121215                                 SORTWS-IDDISTR   '******'                
121216     END-EVALUATE                                                         
121217                                                                          
121218     .                                                                    
121219                                                                          
121220                                                                          
121221 S30A-SKAPA-RIN0-POST  SECTION.                                           
121222                                                                          
121223     INITIALIZE RIN0-RIN-W461RIN0-CTX                                     
121230     MOVE 'RIN'          TO RIN0-RIN-IDPTYP                               
121300     MOVE SORT-IDKUNDNR  TO RIN0-RIN-IDKUNDNR                             
121400     MOVE SORT-IDORDNR7  TO RIN0-RIN-IDORDNR                              
121500     MOVE SORT-IDPRODNR  TO RIN0-RIN-IDPRODNR                             
121600     .                                                                    
121700 S30B-SKAPA-RIO2-POST  SECTION.                                           
121800                                                                          
121900     INITIALIZE RIO2-RIO-W461RIO2                                         
122000     MOVE 'RIO'          TO RIO2-RIO-IDPTYP                               
122100     MOVE SORT-IDORDNR7  TO RIO2-RIO-IDORDNR                              
122200                                                                          
122300*    X(17) VÄNSTERJUSTERAT,   FLYTTAS TILL 9(9)... FIXA                   
122400                                                                          
122500     MOVE SORT-IDARTPRE     TO CIA-IDARTPRE-IN                            
122600     MOVE SORT-IDARTBET     TO CIA-IDARTBET-IN                            
122700     CALL W009CIA USING CIA-W009CIA                                       
122800     IF CIA-KDSVAR = 'F'                                                  
122900        MOVE 'RAD FEL' TO FELTEXT-STR                                     
123000        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
123100     ELSE                                                                 
123200        MOVE CIA-IDARTNR        TO RIO2-RIO-IDARTNR                       
123300     END-IF                                                               
123400                                                                          
123500     MOVE SORT-IDARBREF  TO RIO2-RIO-BERADREF                             
123510                            RIO2-RIO-BEVOLREF                             
123600     MOVE SORT-KVLEVART  TO RIO2-RIO-KVBEART                              
123700                            RIO2-RIO-KVLEVART                             
123800     MOVE SORT-IDBILTYP  TO RIO2-RIO-IDBILTYP                             
123900     MOVE SORT-TIAAAA-PIE  TO RIO2-RIO-TIAAAA                             
124000     MOVE SORT-IDCHASSI-PIE TO RIO2-RIO-IDCHASSI-PIE                      
124100     MOVE SORT-IDVIN     TO RIO2-RIO-IDVIN                                
124200     .                                                                    
124300     EJECT                                                                
124400 Z-FINIT SECTION.                                                         
124500     CLOSE       W41229                                                   
124600                 W41290SE                                                 
124700                 W41290NO                                                 
124800                 W41290DK                                                 
124900                 W41290FI                                                 
125000                 W41290BE                                                 
125100                 W41290GB                                                 
125200                 W41290FR                                                 
125300                 W41290NL                                                 
125400                 W41290EI                                                 
125500                 W41290IT                                                 
125600                 W41290PT                                                 
125700                 W41290PX                                                 
125800                 W41290CH                                                 
125900                 W41290ES                                                 
126000                 W41290DE                                                 
126100                 W41290AT                                                 
126200                 W41290PL                                                 
126300                 W41290SA                                                 
126400                 W41290MY                                                 
126500                 W41290TR                                                 
126600                 W41290KR                                                 
126700                 W41290TX                                                 
126800                 W41290TW                                                 
126900                 W41290MX                                                 
127000                 W41290BR                                                 
127100                 W41290CA                                                 
127200                 W41290JP                                                 
127400                 W41290C1                                                 
127500                 W41290PE                                                 
127600                 W41290RU                                                 
127700                 W41290ZA                                                 
127800                 W41290US                                                 
127810                 W41290CZ                                                 
127820                 W41290HU                                                 
127830                 W41290IN                                                 
127840                 W41290TH                                                 
127850                 W41290AU                                                 
127900                                                                          
128000     SKIP2                                                                
128100     MOVE 'S' TO POSTSUM-OPKOD                                            
128200     CALL POSTSUM USING POSTSUM-PARM                                      
128300     .                                                                    
128400     EJECT                                                                
128500 S01-LAES-W41229  SECTION.                                                
128600     SKIP2                                                                
128700     READ W41229 INTO W41229-INAREA                                       
128800     AT END                                                               
128900        SET END-OF-W41229 TO TRUE                                         
129000                                                                          
129100     NOT AT END                                                           
129200        MOVE 'W41229'          TO POSTSUM-FDNAMN                          
129300        MOVE 'W41229D1'        TO POSTSUM-DDNAMN2                         
129400        CALL POSTSUM           USING POSTSUM-PARM                         
129500     END-READ                                                             
129600     .                                                                    
129700     EJECT                                                                
130700 S31-SORT-RELEASE  SECTION.                                               
130800     SKIP2                                                                
130900     RELEASE SORT-POST FROM SORTWS-AREA                                   
131000     .                                                                    
131100     EJECT                                                                
131200 S32-SORT-RETURN  SECTION.                                                
131300     SKIP2                                                                
131400     RETURN SORTFIL INTO SORTWS-AREA                                      
131500     AT END                                                               
131600         SET END-OF-SORTFIL TO TRUE                                       
131700     .                                                                    
131800     EJECT                                                                
131900 S99-ABEND SECTION.                                                       
132000     SKIP2                                                                
132100     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
132200     .                                                                    
132300     EJECT                                                                
132400* --- IMS SEKTIONER ---                                                   
132500     SKIP3                                                                
132600     EJECT                                                                
132700 IMS-GU-GMTA01 SECTION.                                                   
132800     SKIP2                                                                
132900     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-KEY ')'                         
133000          DELIMITED BY SIZE INTO SSA1                                     
133100     MOVE '  '                  TO GODK-STATUSKODER                       
133200     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA SSA1                      
133300     MOVE GMTA-STATUS-CODE      TO STATUS-WS                              
133400     PERFORM IMS-STATUSKONTROLL                                           
133500     .                                                                    
133600     EJECT                                                                
133700 IMS-STATUSKONTROLL SECTION.                                              
133800     SKIP2                                                                
133900     SET STATUS-IX TO 1                                                   
134000     SEARCH GODK-STATUS                                                   
134100       AT END CALL FELLOG                                                 
134200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
134300     END-SEARCH                                                           
134400     .                                                                    
