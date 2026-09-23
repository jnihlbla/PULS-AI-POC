000101 ID  DIVISION.                                                            
000201 PROGRAM-ID.    W4612000.                                                 
000301*AUTHOR.        STIG MULLER.                                              
000401*DATE-WRITTEN.  NOV 1984.                                                 
000501*                                                                         
000601*    REMARKS.                                                             
000701*                                                                         
000801*    FUNKTION:                                                            
000901*                                                                         
001001*    ABENDKODER:                                                          
001101*                                                                         
001201*        U0016    - OM RETURKOD FRÅN SORT                                 
001301     EJECT                                                                
001401 ENVIRONMENT DIVISION.                                                    
001501     SKIP2                                                                
001601 INPUT-OUTPUT SECTION.                                                    
001701                                                                          
001801 FILE-CONTROL.                                                            
001901     SKIP2                                                                
002001*- - - - - - - - - - - - INFIL:                                           
002101     SELECT INFIL                        ASSIGN TO UT-S-W46120D1.         
002201     SKIP2                                                                
002301*- - - - - - - - - - - - UTFILER:                                         
002401     SELECT W46120                       ASSIGN TO UT-S-W46120D2.         
002501*                  ORDERBEKRÄFTELSE-POSTER                                
002601     SELECT W46127                       ASSIGN TO UT-S-W46120D3.         
002701     SELECT W46129                       ASSIGN TO UT-S-W46120D4.         
002801     SELECT W46130                       ASSIGN TO UT-S-W46120D5.         
002901     SELECT W46172                       ASSIGN TO UT-S-W46120D6.         
003001     SELECT W4612L                       ASSIGN TO UT-S-W46120D7.         
003101     SELECT W46138                       ASSIGN TO UT-S-W46120D8.         
003201     SELECT W46171                       ASSIGN TO UT-S-W46120D9.         
003301     SELECT W46161                       ASSIGN TO UT-S-W46120DA.         
003401     SELECT W46179                       ASSIGN TO UT-S-W46120DB.         
003501     SELECT W46168                       ASSIGN TO UT-S-W46120DC.         
003601     SELECT W46178                       ASSIGN TO UT-S-W46120DD.         
003701     SELECT W46176                       ASSIGN TO UT-S-W46120DE.         
003801     SELECT W46173                       ASSIGN TO UT-S-W46120DF.         
003901     SELECT W46170                       ASSIGN TO UT-S-W46120DG.         
004001     SELECT W46140                       ASSIGN TO UT-S-W46120DH.         
004101     SELECT W46139                       ASSIGN TO UT-S-W46120DI.         
004201     SELECT W46174                       ASSIGN TO UT-S-W46120DJ.         
004301     SELECT W46175                       ASSIGN TO UT-S-W46120DK.         
004401     SELECT W46131                       ASSIGN TO UT-S-W46120DL.         
004501     SELECT W46180                       ASSIGN TO UT-S-W46120DM.         
004601     SELECT W4612Z                       ASSIGN TO UT-S-W46120DN.         
004701     SELECT W46134                       ASSIGN TO UT-S-W46120DO.         
004801     SELECT W4612A                       ASSIGN TO UT-S-W46120DQ.         
004901     SELECT W4612H                       ASSIGN TO UT-S-W46120DP.         
005001     SELECT W46128                       ASSIGN TO UT-S-W46120DS.         
005101     SELECT W4612B                       ASSIGN TO UT-S-W46120DT.         
005201     SELECT W4612C                       ASSIGN TO UT-S-W46120DU.         
005301     SELECT W4612D                       ASSIGN TO UT-S-W46120DV.         
005401     SELECT W4612E                       ASSIGN TO UT-S-W46120DW.         
005501     SELECT W4612F                       ASSIGN TO UT-S-W46120DX.         
005601     SELECT W4612G                       ASSIGN TO UT-S-W46120DY.         
005701     SELECT W4612M                       ASSIGN TO UT-S-W46120DZ.         
005801     SELECT W4612I                       ASSIGN TO UT-S-W46120E1.         
005901     SELECT W4612J                       ASSIGN TO UT-S-W46120E2.         
006001     SELECT W4612K                       ASSIGN TO UT-S-W46120E3.         
006101     SELECT W4612N                       ASSIGN TO UT-S-W46120E4.         
006201     SELECT W4612P                       ASSIGN TO UT-S-W46120E5.         
006301     SELECT W461CN                       ASSIGN TO UT-S-W46120E6.         
006401     SELECT W461ZA                       ASSIGN TO UT-S-W46120E7.         
006501     SELECT W461PT3                      ASSIGN TO UT-S-W46120E8.         
006601     SELECT W461CN1                      ASSIGN TO UT-S-W46120E9.         
006701     SELECT W46126                       ASSIGN TO UT-S-W46120EA.         
006801     SELECT W46122                       ASSIGN TO UT-S-W46120EB.         
006901     SELECT W46123                       ASSIGN TO UT-S-W46120EC.         
007000     SKIP2                                                                
007100*- - - - - - - - - - - - SORTFIL:                                         
007200     SELECT SORTFIL                      ASSIGN TO UT-S-W46120DS.         
007300     EJECT                                                                
007400 DATA DIVISION.                                                           
007500     SKIP2                                                                
007600 FILE SECTION.                                                            
007700     SKIP3                                                                
007800 FD  INFIL                                                                
007900     RECORDING       V                                                    
008000     BLOCK CONTAINS 0.                                                    
008100     SKIP2                                                                
008200*01  FILLER -COPY W461S014   -L.                                          
008300     SKIP2                                                                
008400*01  FILLER -COPY W461S009   -L.                                          
008500     SKIP2                                                                
008600*01  FILLER -COPY W461S001   -L.                                          
008700     SKIP2                                                                
008800*01  FILLER -COPY W461S002   -L.                                          
008900     SKIP2                                                                
009000*01  FILLER -COPY W461S003   -L.                                          
009100     SKIP2                                                                
009200*01  FILLER -COPY W461S004   -L.                                          
009300     SKIP2                                                                
009400*01  FILLER -COPY W461S005   -L.                                          
009500     SKIP2                                                                
009600*01  FILLER -COPY W461S006   -L.                                          
009700     SKIP2                                                                
009800*01  FILLER -COPY W461S007   -L.                                          
009900     SKIP2                                                                
010000*01  FILLER -COPY W461S008   -L.                                          
010100     SKIP2                                                                
010200*01  FILLER -COPY W461S010   -L.                                          
010300     SKIP2                                                                
010400*01  FILLER -COPY W461S011   -L.                                          
010500     SKIP2                                                                
010600*01  FILLER -COPY W461S012   -L.                                          
010700     SKIP2                                                                
010800*01  FILLER -COPY W461S013   -L.                                          
010900     SKIP2                                                                
011000*01  FILLER -COPY W461S015   -L.                                          
011100     SKIP2                                                                
011200*01  FILLER -COPY W461S016   -L.                                          
011300     SKIP2                                                                
011400*01  FILLER -COPY W461S020   -L.                                          
011500     SKIP2                                                                
011600*01  FILLER -COPY W461S021   -L.                                          
011700     SKIP2                                                                
011800*01  FILLER -COPY W461S022   -L.                                          
011900     SKIP2                                                                
012000*01  FILLER -COPY W461S023   -L.                                          
012100     SKIP2                                                                
012200*01  FILLER -COPY W461S024   -L.                                          
012300     SKIP2                                                                
012400*01  FILLER -COPY W461S025   -L.                                          
012500     SKIP2                                                                
012600*01  FILLER -COPY W37183     -L.                                          
012700     SKIP2                                                                
012800*01  FILLER -COPY W461S051   -L.                                          
012900     SKIP2                                                                
013000*01  FILLER -COPY W37119     -L.                                          
013100     EJECT                                                                
013200*01  FILLER -COPY W37167     -L.                                          
013300     EJECT                                                                
013400 FD  W46120                                                               
013500     RECORDING       V                                                    
013600     BLOCK CONTAINS 0.                                                    
013700     SKIP2                                                                
013800*01  OBHUV-POST -COPY W461S002   -L.                                      
013900     SKIP2                                                                
014000*01  OBEN-POST -COPY W461S003   -L.                                       
014100     SKIP2                                                                
014200*01  OBEEN-POST -COPY W461S004   -L.                                      
014300     SKIP2                                                                
014400*01  OBTEXT-POST -COPY W461S005   -L.                                     
014500     SKIP2                                                                
014600*01  OBKVAN-POST -COPY W461S006   -L.                                     
014700     SKIP2                                                                
014800*01  OBLAG-POST -COPY W461S007   -L.                                      
014900     SKIP2                                                                
015000*01  OBSTOP-POST -COPY W461S008   -L.                                     
015100     EJECT                                                                
015200 FD  W46127                                                               
015300     RECORDING       V                                                    
015400     BLOCK CONTAINS 0.                                                    
015500     SKIP2                                                                
015600 01  IMP-IT                   PIC X(80).                                  
015700     EJECT                                                                
015800*01  RID-POST-IT  -COPY W461RIDN   -L.                                    
015900     SKIP2                                                                
016000*01  RIE-POST-IT  -COPY W461RIEN   -L.                                    
016100     SKIP2                                                                
016200*01  RIH-POST-IT  -COPY W461RIHN   -L.                                    
016300     SKIP2                                                                
016400*01  RIO-POST-IT  -COPY W461RIO2 -PRE SOFT-  -L.                          
016500     SKIP2                                                                
016600*01  RKB-POST-IT  -COPY W461RKBN   -L.                                    
016700     SKIP2                                                                
016800*01  RKC-POST-IT  -COPY W461RKCN   -L.                                    
016900     SKIP2                                                                
017000*01  RKD-POST-IT  -COPY W461RKDN   -L.                                    
017100     EJECT                                                                
017200 FD  W46129                                                               
017300     RECORDING       V                                                    
017400     BLOCK CONTAINS 0.                                                    
017500     SKIP2                                                                
017600 01  IMP-FI                  PIC X(80).                                   
017700     EJECT                                                                
017800*01  RID-POST-FI  -COPY W461RIDN   -L.                                    
017900     SKIP2                                                                
018000*01  RIE-POST-FI  -COPY W461RIEN   -L.                                    
018100     SKIP2                                                                
018200*01  RIH-POST-FI  -COPY W461RIHN   -L.                                    
018300     SKIP2                                                                
018400*01  RIO-POST-FI  -COPY W461RIO2 -PRE SOFT-  -L.                          
018500     SKIP2                                                                
018600*01  RKB-POST-FI  -COPY W461RKBN   -L.                                    
018700     SKIP2                                                                
018800*01  RKC-POST-FI  -COPY W461RKCN   -L.                                    
018900     SKIP2                                                                
019000*01  RKD-POST-FI  -COPY W461RKDN   -L.                                    
019100     EJECT                                                                
019200 FD  W46130                                                               
019300     RECORDING       V                                                    
019400     BLOCK CONTAINS 0.                                                    
019500     SKIP2                                                                
019600 01  IMP-BE                   PIC X(80).                                  
019700*01  RID-POST-BE  -COPY W461RIDN   -L.                                    
019800     SKIP2                                                                
019900*01  RIE-POST-BE  -COPY W461RIEN   -L.                                    
020000     SKIP2                                                                
020100*01  RIH-POST-BE  -COPY W461RIHN   -L.                                    
020200     SKIP2                                                                
020300*01  RIO-POST-BE  -COPY W461RIO2 -PRE SOFT-  -L.                          
020400     SKIP2                                                                
020500*01  RKB-POST-BE  -COPY W461RKBN   -L.                                    
020600     SKIP2                                                                
020700*01  RKC-POST-BE  -COPY W461RKCN   -L.                                    
020800     SKIP2                                                                
020900*01  RKD-POST-BE  -COPY W461RKDN   -L.                                    
021000     EJECT                                                                
021100 FD  W46172                                                               
021200     RECORDING       V                                                    
021300     BLOCK CONTAINS 0.                                                    
021400     SKIP2                                                                
021500 01  IMP-CH2078               PIC X(80).                                  
021600*01  RID-POST-CH2078  -COPY W461RIDN   -L.                                
021700     SKIP2                                                                
021800*01  RIE-POST-CH2078  -COPY W461RIEN   -L.                                
021900     SKIP2                                                                
022000*01  RIH-POST-CH2078  -COPY W461RIHN   -L.                                
022100     SKIP2                                                                
022200*01  RIO-POST-CH2078  -COPY W461RIO2 -PRE SOFT-  -L.                      
022300     SKIP2                                                                
022400*01  RKB-POST-CH2078 -COPY W461RKBN -L.                                   
022500     SKIP2                                                                
022600*01  RKC-POST-CH2078 -COPY W461RKCN -L.                                   
022700     SKIP2                                                                
022800*01  RKD-POST-CH2078 -COPY W461RKDN -L.                                   
022900     EJECT                                                                
023000 FD  W46138                                                               
023100     RECORDING       V                                                    
023200     BLOCK CONTAINS 0.                                                    
023300     SKIP2                                                                
023400 01  IMP-US                   PIC X(80).                                  
023500     EJECT                                                                
023600*01  RID-POST-US  -COPY W461RIDN   -L.                                    
023700     SKIP2                                                                
023800*01  RIE-POST-US  -COPY W461RIEN   -L.                                    
023900     SKIP2                                                                
024000*01  RIH-POST-US  -COPY W461RIHN   -L.                                    
024100     SKIP2                                                                
024200*01  RIO-POST-US  -COPY W461RIO2 -PRE SOFT-  -L.                          
024300     SKIP2                                                                
024400*01  RKB-POST-US -COPY W461RKBN -L.                                       
024500     SKIP2                                                                
024600*01  RKC-POST-US -COPY W461RKCN -L.                                       
024700     SKIP2                                                                
024800*01  RKD-POST-US -COPY W461RKDN -L.                                       
024900     EJECT                                                                
025000 FD  W46171                                                               
025100     RECORDING       V                                                    
025200     BLOCK CONTAINS 0.                                                    
025300     SKIP2                                                                
025400 01  IMP-DE                   PIC X(80).                                  
025500     EJECT                                                                
025600*01  RID-POST-DE  -COPY W461RIDN   -L.                                    
025700     SKIP2                                                                
025800*01  RIE-POST-DE  -COPY W461RIEN   -L.                                    
025900     SKIP2                                                                
026000*01  RIH-POST-DE  -COPY W461RIHN   -L.                                    
026100     SKIP2                                                                
026200*01  RIO-POST-DE  -COPY W461RIO2 -PRE SOFT-  -L.                          
026300     SKIP2                                                                
026400*01  RKB-POST-DE -COPY W461RKBN -L.                                       
026500     SKIP2                                                                
026600*01  RKC-POST-DE -COPY W461RKCN -L.                                       
026700     SKIP2                                                                
026800*01  RKD-POST-DE -COPY W461RKDN -L.                                       
026900     EJECT                                                                
027000 FD  W46161                                                               
027100     RECORDING       V                                                    
027200     BLOCK CONTAINS 0.                                                    
027300     SKIP2                                                                
027400 01  IMP-NL                   PIC X(80).                                  
027500*01  RID-POST-NL  -COPY W461RIDN   -L.                                    
027600     SKIP2                                                                
027700*01  RIE-POST-NL  -COPY W461RIEN   -L.                                    
027800     SKIP2                                                                
027900*01  RIH-POST-NL  -COPY W461RIHN   -L.                                    
028000     SKIP2                                                                
028100*01  RIO-POST-NL  -COPY W461RIO2 -PRE SOFT-  -L.                          
028200     SKIP2                                                                
028300*01  RKB-POST-NL -COPY W461RKBN -L.                                       
028400     SKIP2                                                                
028500*01  RKC-POST-NL -COPY W461RKCN -L.                                       
028600     SKIP2                                                                
028700*01  RKD-POST-NL -COPY W461RKDN -L.                                       
028800     EJECT                                                                
028900 FD  W46179                                                               
029000     RECORDING       V                                                    
029100     BLOCK CONTAINS 0.                                                    
029200     SKIP2                                                                
029300 01  IMP-ES                   PIC X(80).                                  
029400     EJECT                                                                
029500*01  RID-POST-ES  -COPY W461RIDN   -L.                                    
029600     SKIP2                                                                
029700*01  RIE-POST-ES  -COPY W461RIEN   -L.                                    
029800     SKIP2                                                                
029900*01  RIH-POST-ES  -COPY W461RIHN   -L.                                    
030000     SKIP2                                                                
030100*01  RIO-POST-ES  -COPY W461RIO2 -PRE SOFT-  -L.                          
030200     SKIP2                                                                
030300*01  RKB-POST-ES -COPY W461RKBN -L.                                       
030400     SKIP2                                                                
030500*01  RKC-POST-ES -COPY W461RKCN -L.                                       
030600     SKIP2                                                                
030700*01  RKD-POST-ES -COPY W461RKDN -L.                                       
030800     EJECT                                                                
030900 FD  W46168                                                               
031000     RECORDING       V                                                    
031100     BLOCK CONTAINS 0.                                                    
031200     SKIP2                                                                
031300 01  IMP-AT                   PIC X(80).                                  
031400     EJECT                                                                
031500*01  RID-POST-AT  -COPY W461RIDN   -L.                                    
031600     SKIP2                                                                
031700*01  RIE-POST-AT  -COPY W461RIEN   -L.                                    
031800     SKIP2                                                                
031900*01  RIH-POST-AT  -COPY W461RIHN   -L.                                    
032000     SKIP2                                                                
032100*01  RIO-POST-AT  -COPY W461RIO2 -PRE SOFT-  -L.                          
032200     SKIP2                                                                
032300*01  RKB-POST-AT -COPY W461RKBN -L.                                       
032400     SKIP2                                                                
032500*01  RKC-POST-AT -COPY W461RKCN -L.                                       
032600     SKIP2                                                                
032700*01  RKD-POST-AT -COPY W461RKDN -L.                                       
032800     EJECT                                                                
032900 FD  W46178                                                               
033000     RECORDING       V                                                    
033100     BLOCK CONTAINS 0.                                                    
033200     SKIP2                                                                
033300 01  IMP-SA                   PIC X(80).                                  
033400     EJECT                                                                
033500*01  RID-POST-SA  -COPY W461RIDN   -L.                                    
033600     SKIP2                                                                
033700*01  RIE-POST-SA  -COPY W461RIEN   -L.                                    
033800     SKIP2                                                                
033900*01  RIH-POST-SA  -COPY W461RIHN   -L.                                    
034000     SKIP2                                                                
034100*01  RIO-POST-SA  -COPY W461RIO1   -L.                                    
034200     SKIP2                                                                
034300*01  RKB-POST-SA -COPY W461RKBN -L.                                       
034400     SKIP2                                                                
034500*01  RKC-POST-SA -COPY W461RKCN -L.                                       
034600     SKIP2                                                                
034700*01  RKD-POST-SA -COPY W461RKDN -L.                                       
034800     EJECT                                                                
034900 FD  W46176                                                               
035000     RECORDING       V                                                    
035100     BLOCK CONTAINS 0.                                                    
035200     SKIP2                                                                
035300 01  IMP-PE                   PIC X(80).                                  
035400     EJECT                                                                
035500*01  RID-POST-PE  -COPY W461RIDN   -L.                                    
035600     SKIP2                                                                
035700*01  RIE-POST-PE  -COPY W461RIEN   -L.                                    
035800     SKIP2                                                                
035900*01  RIH-POST-PE  -COPY W461RIHN   -L.                                    
036000     SKIP2                                                                
036100*01  RIO-POST-PE  -COPY W461RIO1   -L.                                    
036200     SKIP2                                                                
036300*01  RKB-POST-PE -COPY W461RKBN -L.                                       
036400     SKIP2                                                                
036500*01  RKC-POST-PE -COPY W461RKCN -L.                                       
036600     SKIP2                                                                
036700*01  RKD-POST-PE -COPY W461RKDN -L.                                       
036800     EJECT                                                                
036900 FD  W46173                                                               
037000     RECORDING       V                                                    
037100     BLOCK CONTAINS 0.                                                    
037200     SKIP2                                                                
037300 01  IMP-FR                   PIC X(80).                                  
037400     EJECT                                                                
037500*01  RID-POST-FR  -COPY W461RIDN   -L.                                    
037600     SKIP2                                                                
037700*01  RIE-POST-FR  -COPY W461RIEN   -L.                                    
037800     SKIP2                                                                
037900*01  RIH-POST-FR  -COPY W461RIHN   -L.                                    
038000     SKIP2                                                                
038100*01  RIO-POST-FR  -COPY W461RIO2 -PRE SOFT-  -L.                          
038200     SKIP2                                                                
038300*01  RKB-POST-FR -COPY W461RKBN -L.                                       
038400     SKIP2                                                                
038500*01  RKC-POST-FR -COPY W461RKCN -L.                                       
038600     SKIP2                                                                
038700*01  RKD-POST-FR -COPY W461RKDN -L.                                       
038800     EJECT                                                                
038900 FD  W46170                                                               
039000     RECORDING       V                                                    
039100     BLOCK CONTAINS 0.                                                    
039200     SKIP2                                                                
039300 01  IMP-SE                   PIC X(80).                                  
039400     EJECT                                                                
039500*01  RID-POST-SE  -COPY W461RIDN   -L.                                    
039600     SKIP2                                                                
039700*01  RIE-POST-SE  -COPY W461RIEN   -L.                                    
039800     SKIP2                                                                
039900*01  RIH-POST-SE  -COPY W461RIHN   -L.                                    
040000     SKIP2                                                                
040100*01  RIO-POST-SE  -COPY W461RIO2 -PRE SOFT-  -L.                          
040200     SKIP2                                                                
040300*01  RKB-POST-SE -COPY W461RKBN -L.                                       
040400     SKIP2                                                                
040500*01  RKC-POST-SE -COPY W461RKCN -L.                                       
040600     SKIP2                                                                
040700*01  RKD-POST-SE -COPY W461RKDN -L.                                       
040800     EJECT                                                                
040900 FD  W46140                                                               
041000     RECORDING       V                                                    
041100     BLOCK CONTAINS 0.                                                    
041200     SKIP2                                                                
041300 01  IMP-DK                   PIC X(80).                                  
041400     EJECT                                                                
041500*01  RID-POST-DK  -COPY W461RIDN   -L.                                    
041600     SKIP2                                                                
041700*01  RIE-POST-DK  -COPY W461RIEN   -L.                                    
041800     SKIP2                                                                
041900*01  RIH-POST-DK  -COPY W461RIHN   -L.                                    
042000     SKIP2                                                                
042100*01  RIO-POST-DK  -COPY W461RIO2 -PRE SOFT-  -L.                          
042200     SKIP2                                                                
042300*01  RKB-POST-DK -COPY W461RKBN -L.                                       
042400     SKIP2                                                                
042500*01  RKC-POST-DK -COPY W461RKCN -L.                                       
042600     SKIP2                                                                
042700*01  RKD-POST-DK -COPY W461RKDN -L.                                       
042800     EJECT                                                                
042900 FD  W46139                                                               
043000     RECORDING       V                                                    
043100     BLOCK CONTAINS 0.                                                    
043200     SKIP2                                                                
043300 01  IMP-NO                   PIC X(80).                                  
043400     EJECT                                                                
043500*01  RID-POST-NO  -COPY W461RIDN   -L.                                    
043600     SKIP2                                                                
043700*01  RIE-POST-NO  -COPY W461RIEN   -L.                                    
043800     SKIP2                                                                
043900*01  RIH-POST-NO  -COPY W461RIHN   -L.                                    
044000     SKIP2                                                                
044100*01  RIO-POST-NO  -COPY W461RIO2 -PRE SOFT-  -L.                          
044200     SKIP2                                                                
044300*01  RKB-POST-NO -COPY W461RKBN -L.                                       
044400     SKIP2                                                                
044500*01  RKC-POST-NO -COPY W461RKCN -L.                                       
044600     SKIP2                                                                
044700*01  RKD-POST-NO -COPY W461RKDN -L.                                       
044800     EJECT                                                                
044900 FD  W46174                                                               
045000     RECORDING       V                                                    
045100     BLOCK CONTAINS 0.                                                    
045200     SKIP2                                                                
045300 01  IMP-CH2070               PIC X(80).                                  
045400     EJECT                                                                
045500*01  RID-POST-CH2070  -COPY W461RIDN   -L.                                
045600     SKIP2                                                                
045700*01  RIE-POST-CH2070  -COPY W461RIEN   -L.                                
045800     SKIP2                                                                
045900*01  RIH-POST-CH2070  -COPY W461RIHN   -L.                                
046000     SKIP2                                                                
046100*01  RIO-POST-CH2070  -COPY W461RIO2 -PRE SOFT-  -L.                      
046200     SKIP2                                                                
046300*01  RKB-POST-CH2070 -COPY W461RKBN -L.                                   
046400     SKIP2                                                                
046500*01  RKC-POST-CH2070 -COPY W461RKCN -L.                                   
046600     SKIP2                                                                
046700*01  RKD-POST-CH2070 -COPY W461RKDN -L.                                   
046800     EJECT                                                                
046900 FD  W46175                                                               
047000     RECORDING       V                                                    
047100     BLOCK CONTAINS 0.                                                    
047200     SKIP2                                                                
047300 01  IMP-BR                   PIC X(80).                                  
047400     EJECT                                                                
047500*01  RID-POST-BR  -COPY W461RIDN   -L.                                    
047600     SKIP2                                                                
047700*01  RIE-POST-BR  -COPY W461RIEN   -L.                                    
047800     SKIP2                                                                
047900*01  RIH-POST-BR  -COPY W461RIHN   -L.                                    
048000     SKIP2                                                                
048100*01  RIO-POST-BR  -COPY W461RIO1   -L.                                    
048200     SKIP2                                                                
048300*01  RKB-POST-BR -COPY W461RKBN -L.                                       
048400     SKIP2                                                                
048500*01  RKC-POST-BR -COPY W461RKCN -L.                                       
048600     SKIP2                                                                
048700*01  RKD-POST-BR -COPY W461RKDN -L.                                       
048800     EJECT                                                                
048900 FD  W46131                                                               
049000     RECORDING       V                                                    
049100     BLOCK CONTAINS 0.                                                    
049200     SKIP2                                                                
049300 01  IMP-AU7836               PIC X(80).                                  
049400     EJECT                                                                
049500*01  RID-POST-AU7836  -COPY W461RIDN   -L.                                
049600     SKIP2                                                                
049700*01  RIE-POST-AU7836  -COPY W461RIEN   -L.                                
049800     SKIP2                                                                
049900*01  RIH-POST-AU7836  -COPY W461RIHN   -L.                                
050000     SKIP2                                                                
050100*01  RIO-POST-AU7836  -COPY W461RIO2 -PRE SOFT-  -L.                      
050200     SKIP2                                                                
050300*01  RKB-POST-AU7836 -COPY W461RKBN -L.                                   
050400     SKIP2                                                                
050500*01  RKC-POST-AU7836 -COPY W461RKCN -L.                                   
050600     SKIP2                                                                
050700*01  RKD-POST-AU7836 -COPY W461RKDN -L.                                   
050800     EJECT                                                                
050900 FD  W46180                                                               
051000     RECORDING       V                                                    
051100     BLOCK CONTAINS 0.                                                    
051200     SKIP2                                                                
051300 01  IMP-AU7838               PIC X(80).                                  
051400     EJECT                                                                
051500*01  RID-POST-AU7838  -COPY W461RIDN   -L.                                
051600     SKIP2                                                                
051700*01  RIE-POST-AU7838  -COPY W461RIEN   -L.                                
051800     SKIP2                                                                
051900*01  RIH-POST-AU7838  -COPY W461RIHN   -L.                                
052000     SKIP2                                                                
052100*01  RIO-POST-AU7838  -COPY W461RIO2 -PRE SOFT-  -L.                      
052200     SKIP2                                                                
052300*01  RKB-POST-AU7838 -COPY W461RKBN -L.                                   
052400     SKIP2                                                                
052500*01  RKC-POST-AU7838 -COPY W461RKCN -L.                                   
052600     SKIP2                                                                
052700*01  RKD-POST-AU7838 -COPY W461RKDN -L.                                   
052800     EJECT                                                                
052900 FD  W4612Z                                                               
053000     RECORDING       V                                                    
053100     BLOCK CONTAINS 0.                                                    
053200     SKIP2                                                                
053300 01  IMP-FI1091              PIC X(80).                                   
053400     EJECT                                                                
053500*01  RID-POST-FI1091  -COPY W461RIDN   -L.                                
053600     SKIP2                                                                
053700*01  RIE-POST-FI1091  -COPY W461RIEN   -L.                                
053800     SKIP2                                                                
053900*01  RIH-POST-FI1091  -COPY W461RIHN   -L.                                
054000     SKIP2                                                                
054100*01  RIO-POST-FI1091  -COPY W461RIO2 -PRE SOFT-  -L.                      
054200     SKIP2                                                                
054300*01  RKB-POST-FI1091 -COPY W461RKBN -L.                                   
054400     SKIP2                                                                
054500*01  RKC-POST-FI1091 -COPY W461RKCN -L.                                   
054600     SKIP2                                                                
054700*01  RKD-POST-FI1091 -COPY W461RKDN -L.                                   
054800     EJECT                                                                
054900 FD  W46134                                                               
055000     RECORDING       V                                                    
055100     BLOCK CONTAINS 0.                                                    
055200     SKIP2                                                                
055300 01  IMP-TW                   PIC X(80).                                  
055400     EJECT                                                                
055500*01  RID-POST-TW  -COPY W461RIDN   -L.                                    
055600     SKIP2                                                                
055700*01  RIE-POST-TW  -COPY W461RIEN   -L.                                    
055800     SKIP2                                                                
055900*01  RIH-POST-TW  -COPY W461RIHN   -L.                                    
056000     SKIP2                                                                
056100*01  RIO-POST-TW  -COPY W461RIO2 -PRE SOFT-  -L.                          
056200     SKIP2                                                                
056300*01  RKB-POST-TW -COPY W461RKBN -L.                                       
056400     SKIP2                                                                
056500*01  RKC-POST-TW -COPY W461RKCN -L.                                       
056600     SKIP2                                                                
056700*01  RKD-POST-TW -COPY W461RKDN -L.                                       
056800     EJECT                                                                
056900 FD  W4612A                                                               
057000     RECORDING       V                                                    
057100     BLOCK CONTAINS 0.                                                    
057200     SKIP2                                                                
057300 01  IMP-TW2                  PIC X(80).                                  
057400     EJECT                                                                
057500*01  RID-POST-TW2  -COPY W461RIDN   -L.                                   
057600     SKIP2                                                                
057700*01  RIE-POST-TW2  -COPY W461RIEN   -L.                                   
057800     SKIP2                                                                
057900*01  RIH-POST-TW2  -COPY W461RIHN   -L.                                   
058000     SKIP2                                                                
058100*01  RIO-POST-TW2  -COPY W461RIO2 -PRE SOFT-  -L.                         
058200     SKIP2                                                                
058300*01  RKB-POST-TW2 -COPY W461RKBN -L.                                      
058400     SKIP2                                                                
058500*01  RKC-POST-TW2 -COPY W461RKCN -L.                                      
058600     SKIP2                                                                
058700*01  RKD-POST-TW2 -COPY W461RKDN -L.                                      
058800     EJECT                                                                
058900 FD  W4612H                                                               
059000     RECORDING       V                                                    
059100     BLOCK CONTAINS 0.                                                    
059200     SKIP2                                                                
059300 01  IMP-JP                   PIC X(80).                                  
059400     EJECT                                                                
059500*01  RID-POST-JP -COPY W461RIDN   -L.                                     
059600     SKIP2                                                                
059700*01  RIE-POST-JP  -COPY W461RIEN   -L.                                    
059800     SKIP2                                                                
059900*01  RIH-POST-JP  -COPY W461RIHN   -L.                                    
060000     SKIP2                                                                
060100*01  RIO-POST-JP  -COPY W461RIO2 -PRE SOFT-  -L.                          
060200     SKIP2                                                                
060300*01  RKB-POST-JP -COPY W461RKBN -L.                                       
060400     SKIP2                                                                
060500*01  RKC-POST-JP -COPY W461RKCN -L.                                       
060600     SKIP2                                                                
060700*01  RKD-POST-JP -COPY W461RKDN -L.                                       
060800     EJECT                                                                
060900 FD  W46128                                                               
061000     RECORDING       V                                                    
061100     BLOCK CONTAINS 0.                                                    
061200     SKIP2                                                                
061300 01  IMP-JP5220               PIC X(80).                                  
061400     SKIP3                                                                
061500*01  RID-POST-JP5220 -COPY W461RIDN -L.                                   
061600     SKIP2                                                                
061700*01  RIE-POST-JP5220 -COPY W461RIEN -L.                                   
061800     SKIP2                                                                
061900*01  RIH-POST-JP5220 -COPY W461RIHN -L.                                   
062000     SKIP2                                                                
062100*01  RIO-POST-JP5220 -COPY W461RIO2 -PRE SOFT- -L.                        
062200     SKIP2                                                                
062300*01  RKB-POST-JP5220 -COPY W461RKBN -L.                                   
062400     SKIP2                                                                
062500*01  RKC-POST-JP5220 -COPY W461RKCN -L.                                   
062600     SKIP2                                                                
062700*01  RKD-POST-JP5220 -COPY W461RKDN -L.                                   
062800     EJECT                                                                
062900 FD  W4612B                                                               
063000     RECORDING       V                                                    
063100     BLOCK CONTAINS 0.                                                    
063200     SKIP2                                                                
063300 01  IMP-GB1378               PIC X(80).                                  
063400     SKIP2                                                                
063500*01  RID-POST-GB1378  -COPY W461RIDN   -L.                                
063600     SKIP2                                                                
063700*01  RIE-POST-GB1378  -COPY W461RIEN   -L.                                
063800     SKIP2                                                                
063900*01  RIH-POST-GB1378  -COPY W461RIHN   -L.                                
064000     SKIP2                                                                
064100*01  RIO-POST-GB1378  -COPY W461RIO2 -PRE SOFT-  -L.                      
064200     SKIP2                                                                
064300*01  RKB-POST-GB1378 -COPY W461RKBN -L.                                   
064400     SKIP2                                                                
064500*01  RKC-POST-GB1378 -COPY W461RKCN -L.                                   
064600     SKIP2                                                                
064700*01  RKD-POST-GB1378 -COPY W461RKDN -L.                                   
064800     EJECT                                                                
064900 FD  W4612C                                                               
065000     RECORDING       V                                                    
065100     BLOCK CONTAINS 0.                                                    
065200     SKIP2                                                                
065300 01  IMP-PL                   PIC X(80).                                  
065400     EJECT                                                                
065500*01  RID-POST-PL  -COPY W461RIDN   -L.                                    
065600     SKIP2                                                                
065700*01  RIE-POST-PL  -COPY W461RIEN   -L.                                    
065800     SKIP2                                                                
065900*01  RIH-POST-PL  -COPY W461RIHN   -L.                                    
066000     SKIP2                                                                
066100*01  RIO-POST-PL  -COPY W461RIO2 -PRE SOFT-  -L.                          
066200     SKIP2                                                                
066300*01  RKB-POST-PL -COPY W461RKBN -L.                                       
066400     SKIP2                                                                
066500*01  RKC-POST-PL -COPY W461RKCN -L.                                       
066600     SKIP2                                                                
066700*01  RKD-POST-PL -COPY W461RKDN -L.                                       
066800     EJECT                                                                
066900 FD  W4612D                                                               
067000     RECORDING       V                                                    
067100     BLOCK CONTAINS 0.                                                    
067200     SKIP2                                                                
067300 01  IMP-CAN               PIC X(80).                                     
067400     EJECT                                                                
067500*01  RID-POST-CAN  -COPY W461RIDN   -L.                                   
067600     SKIP2                                                                
067700*01  RIE-POST-CAN  -COPY W461RIEN   -L.                                   
067800     SKIP2                                                                
067900*01  RIH-POST-CAN  -COPY W461RIHN   -L.                                   
068000     SKIP2                                                                
068100*01  RIO-POST-CAN  -COPY W461RIO2 -PRE SOFT-  -L.                         
068200     SKIP2                                                                
068300*01  RKB-POST-CAN -COPY W461RKBN -L.                                      
068400     SKIP2                                                                
068500*01  RKC-POST-CAN -COPY W461RKCN -L.                                      
068600     SKIP2                                                                
068700*01  RKD-POST-CAN -COPY W461RKDN -L.                                      
068800     EJECT                                                                
068900 FD  W4612E                                                               
069000     RECORDING       V                                                    
069100     BLOCK CONTAINS 0.                                                    
069200     SKIP2                                                                
069300 01  IMP-USA                  PIC X(80).                                  
069400     EJECT                                                                
069500*01  RID-POST-USA  -COPY W461RIDN   -L.                                   
069600     SKIP2                                                                
069700*01  RIE-POST-USA  -COPY W461RIEN   -L.                                   
069800     SKIP2                                                                
069900*01  RIH-POST-USA  -COPY W461RIHN   -L.                                   
070000     SKIP2                                                                
070100*01  RIO-POST-USA  -COPY W461RIO2 -PRE SOFT-  -L.                         
070200     SKIP2                                                                
070300*01  RKB-POST-USA -COPY W461RKBN -L.                                      
070400     SKIP2                                                                
070500*01  RKC-POST-USA -COPY W461RKCN -L.                                      
070600     SKIP2                                                                
070700*01  RKD-POST-USA -COPY W461RKDN -L.                                      
070800     EJECT                                                                
070900 FD  W4612F                                                               
071000     RECORDING       V                                                    
071100     BLOCK CONTAINS 0.                                                    
071200     SKIP2                                                                
071300 01  IMP-KR                   PIC X(80).                                  
071400     EJECT                                                                
071500*01  RID-POST-KR   -COPY W461RIDN   -L.                                   
071600     SKIP2                                                                
071700*01  RIE-POST-KR   -COPY W461RIEN   -L.                                   
071800     SKIP2                                                                
071900*01  RIH-POST-KR   -COPY W461RIHN   -L.                                   
072000     SKIP2                                                                
072100*01  RIO-POST-KR  -COPY W461RIO2 -PRE SOFT-  -L.                          
072200     SKIP2                                                                
072300*01  RKB-POST-KR -COPY W461RKBN -L.                                       
072400     SKIP2                                                                
072500*01  RKC-POST-KR -COPY W461RKCN -L.                                       
072600     SKIP2                                                                
072700*01  RKD-POST-KR -COPY W461RKDN -L.                                       
072800     EJECT                                                                
072900 FD  W4612G                                                               
073000     RECORDING       V                                                    
073100     BLOCK CONTAINS 0.                                                    
073200     SKIP2                                                                
073300 01  IMP-PT                   PIC X(80).                                  
073400     EJECT                                                                
073500*01  RID-POST-PT   -COPY W461RIDN   -L.                                   
073600     SKIP2                                                                
073700*01  RIE-POST-PT   -COPY W461RIEN   -L.                                   
073800     SKIP2                                                                
073900*01  RIH-POST-PT   -COPY W461RIHN   -L.                                   
074000     SKIP2                                                                
074100*01  RIO-POST-PT  -COPY W461RIO2 -PRE SOFT-  -L.                          
074200     SKIP2                                                                
074300*01  RKB-POST-PT -COPY W461RKBN -L.                                       
074400     SKIP2                                                                
074500*01  RKC-POST-PT -COPY W461RKCN -L.                                       
074600     SKIP2                                                                
074700*01  RKD-POST-PT -COPY W461RKDN -L.                                       
074800     EJECT                                                                
074900 FD  W4612I                                                               
075000     RECORDING       V                                                    
075100     BLOCK CONTAINS 0.                                                    
075200     SKIP2                                                                
075300 01  IMP-MY                   PIC X(80).                                  
075400     EJECT                                                                
075500*01  RID-POST-MY   -COPY W461RIDN   -L.                                   
075600     SKIP2                                                                
075700*01  RIE-POST-MY   -COPY W461RIEN   -L.                                   
075800     SKIP2                                                                
075900*01  RIH-POST-MY   -COPY W461RIHN   -L.                                   
076000     SKIP2                                                                
076100*01  RIO-POST-MY  -COPY W461RIO2 -PRE SOFT-  -L.                          
076200     SKIP2                                                                
076300*01  RKB-POST-MY -COPY W461RKBN -L.                                       
076400     SKIP2                                                                
076500*01  RKC-POST-MY -COPY W461RKCN -L.                                       
076600     SKIP2                                                                
076700*01  RKD-POST-MY -COPY W461RKDN -L.                                       
076800     EJECT                                                                
076900 FD  W4612J                                                               
077000     RECORDING       V                                                    
077100     BLOCK CONTAINS 0.                                                    
077200     SKIP2                                                                
077300 01  IMP-TH                   PIC X(80).                                  
077400     EJECT                                                                
077500*01  RID-POST-TH   -COPY W461RIDN   -L.                                   
077600     SKIP2                                                                
077700*01  RIE-POST-TH   -COPY W461RIEN   -L.                                   
077800     SKIP2                                                                
077900*01  RIH-POST-TH   -COPY W461RIHN   -L.                                   
078000     SKIP2                                                                
078100*01  RIO-POST-TH  -COPY W461RIO2 -PRE SOFT-  -L.                          
078200     SKIP2                                                                
078300*01  RKB-POST-TH -COPY W461RKBN -L.                                       
078400     SKIP2                                                                
078500*01  RKC-POST-TH -COPY W461RKCN -L.                                       
078600     SKIP2                                                                
078700*01  RKD-POST-TH -COPY W461RKDN -L.                                       
078800     EJECT                                                                
078900 FD  W4612K                                                               
079000     RECORDING       V                                                    
079100     BLOCK CONTAINS 0.                                                    
079200     SKIP2                                                                
079300 01  IMP-IE                   PIC X(80).                                  
079400     SKIP2                                                                
079500*01  RID-POST-IE      -COPY W461RIDN   -L.                                
079600     SKIP2                                                                
079700*01  RIE-POST-IE      -COPY W461RIEN   -L.                                
079800     SKIP2                                                                
079900*01  RIH-POST-IE      -COPY W461RIHN   -L.                                
080000     SKIP2                                                                
080100*01  RIO-POST-IE      -COPY W461RIO2 -PRE SOFT-  -L.                      
080200     SKIP2                                                                
080300*01  RKB-POST-IE -COPY W461RKBN -L.                                       
080400     SKIP2                                                                
080500*01  RKC-POST-IE -COPY W461RKCN -L.                                       
080600     SKIP2                                                                
080700*01  RKD-POST-IE -COPY W461RKDN -L.                                       
080800     EJECT                                                                
080900 FD  W4612L                                                               
081000     RECORDING       V                                                    
081100     BLOCK CONTAINS 0.                                                    
081200     SKIP2                                                                
081300 01  IMP-BR-NEW               PIC X(80).                                  
081400     SKIP2                                                                
081500*01  RID-POST-BR-NEW  -COPY W461RIDN   -L.                                
081600     SKIP2                                                                
081700*01  RIE-POST-BR-NEW  -COPY W461RIEN   -L.                                
081800     SKIP2                                                                
081900*01  RIH-POST-BR-NEW  -COPY W461RIHN   -L.                                
082000     SKIP2                                                                
082100*01  RIO-POST-BR-NEW  -COPY W461RIO2 -PRE SOFT-  -L.                      
082200     SKIP2                                                                
082300*01  RKB-POST-BR-NEW -COPY W461RKBN -L.                                   
082400     SKIP2                                                                
082500*01  RKC-POST-BR-NEW -COPY W461RKCN -L.                                   
082600     SKIP2                                                                
082700*01  RKD-POST-BR-NEW -COPY W461RKDN -L.                                   
082800     EJECT                                                                
082900 FD  W4612M                                                               
083000     RECORDING       V                                                    
083100     BLOCK CONTAINS 0.                                                    
083200     SKIP2                                                                
083300 01  IMP-MX                   PIC X(80).                                  
083400     SKIP2                                                                
083500*01  RID-POST-MX      -COPY W461RIDN   -L.                                
083600     SKIP2                                                                
083700*01  RIE-POST-MX      -COPY W461RIEN   -L.                                
083800     SKIP2                                                                
083900*01  RIH-POST-MX      -COPY W461RIHN   -L.                                
084000     SKIP2                                                                
084100*01  RIO-POST-MX      -COPY W461RIO2 -PRE SOFT-  -L.                      
084200     SKIP2                                                                
084300*01  RKB-POST-MX -COPY W461RKBN -L.                                       
084400     SKIP2                                                                
084500*01  RKC-POST-MX -COPY W461RKCN -L.                                       
084600     SKIP2                                                                
084700*01  RKD-POST-MX -COPY W461RKDN -L.                                       
084800     EJECT                                                                
084900 FD  W4612N                                                               
085000     RECORDING       V                                                    
085100     BLOCK CONTAINS 0.                                                    
085200     SKIP2                                                                
085300 01  IMP-TR                   PIC X(80).                                  
085400     SKIP2                                                                
085500*01  RID-POST-TR      -COPY W461RIDN   -L.                                
085600     SKIP2                                                                
085700*01  RIE-POST-TR      -COPY W461RIEN   -L.                                
085800     SKIP2                                                                
085900*01  RIH-POST-TR      -COPY W461RIHN   -L.                                
086000     SKIP2                                                                
086100*01  RIO-POST-TR      -COPY W461RIO2 -PRE SOFT-  -L.                      
086200     SKIP2                                                                
086300*01  RKB-POST-TR -COPY W461RKBN -L.                                       
086400     SKIP2                                                                
086500*01  RKC-POST-TR -COPY W461RKCN -L.                                       
086600     SKIP2                                                                
086700*01  RKD-POST-TR -COPY W461RKDN -L.                                       
086800     EJECT                                                                
086900 FD  W4612P                                                               
087000     RECORDING       V                                                    
087100     BLOCK CONTAINS 0.                                                    
087200     SKIP2                                                                
087300 01  IMP-RU                   PIC X(80).                                  
087400     SKIP2                                                                
087500*01  RID-POST-RU      -COPY W461RIDN   -L.                                
087600     SKIP2                                                                
087700*01  RIE-POST-RU      -COPY W461RIEN   -L.                                
087800     SKIP2                                                                
087900*01  RIH-POST-RU      -COPY W461RIHN   -L.                                
088000     SKIP2                                                                
088100*01  RIO-POST-RU      -COPY W461RIO2 -PRE SOFT-  -L.                      
088200     SKIP2                                                                
088300*01  RKB-POST-RU      -COPY W461RKBN -L.                                  
088400     SKIP2                                                                
088500*01  RKC-POST-RU      -COPY W461RKCN -L.                                  
088600     SKIP2                                                                
088700*01  RKD-POST-RU      -COPY W461RKDN -L.                                  
088800     EJECT                                                                
088900 FD  W461CN                                                               
089000     RECORDING       V                                                    
089100     BLOCK CONTAINS 0.                                                    
089200     SKIP2                                                                
089300 01  IMP-CN                   PIC X(80).                                  
089400     SKIP2                                                                
089500*01  RID-POST-CN      -COPY W461RIDN   -L.                                
089600     SKIP2                                                                
089700*01  RIE-POST-CN      -COPY W461RIEN   -L.                                
089800     SKIP2                                                                
089900*01  RIH-POST-CN      -COPY W461RIHN   -L.                                
090000     SKIP2                                                                
090100*01  RIO-POST-CN      -COPY W461RIO2 -PRE SOFT-  -L.                      
090200     SKIP2                                                                
090300*01  RKB-POST-CN      -COPY W461RKBN -L.                                  
090400     SKIP2                                                                
090500*01  RKC-POST-CN      -COPY W461RKCN -L.                                  
090600     SKIP2                                                                
090700*01  RKD-POST-CN      -COPY W461RKDN -L.                                  
090800     EJECT                                                                
090900 FD  W461CN1                                                              
091000     RECORDING       V                                                    
091100     BLOCK CONTAINS 0.                                                    
091200     SKIP2                                                                
091300 01  IMP-CN1                  PIC X(80).                                  
091400     SKIP2                                                                
091500*01  RID-POST-CN1     -COPY W461RIDN   -L.                                
091600     SKIP2                                                                
091700*01  RIE-POST-CN1     -COPY W461RIEN   -L.                                
091800     SKIP2                                                                
091900*01  RIH-POST-CN1     -COPY W461RIHN   -L.                                
092000     SKIP2                                                                
092100*01  RIO-POST-CN1     -COPY W461RIO2 -PRE SOFT-  -L.                      
092200     SKIP2                                                                
092300*01  RKB-POST-CN1     -COPY W461RKBN -L.                                  
092400     SKIP2                                                                
092500*01  RKC-POST-CN1     -COPY W461RKCN -L.                                  
092600     SKIP2                                                                
092700*01  RKD-POST-CN1     -COPY W461RKDN -L.                                  
092800     EJECT                                                                
092900 FD  W461ZA                                                               
093000     RECORDING       V                                                    
093100     BLOCK CONTAINS 0.                                                    
093200     SKIP2                                                                
093300 01  IMP-ZA                   PIC X(80).                                  
093400     SKIP2                                                                
093500*01  RID-POST-ZA      -COPY W461RIDN   -L.                                
093600     SKIP2                                                                
093700*01  RIE-POST-ZA      -COPY W461RIEN   -L.                                
093800     SKIP2                                                                
093900*01  RIH-POST-ZA      -COPY W461RIHN   -L.                                
094000     SKIP2                                                                
094100*01  RIO-POST-ZA      -COPY W461RIO2 -PRE SOFT-  -L.                      
094200     SKIP2                                                                
094300*01  RKB-POST-ZA      -COPY W461RKBN -L.                                  
094400     SKIP2                                                                
094500*01  RKC-POST-ZA      -COPY W461RKCN -L.                                  
094600     SKIP2                                                                
094700*01  RKD-POST-ZA      -COPY W461RKDN -L.                                  
094800     EJECT                                                                
094900 FD  W461PT3                                                              
095000     RECORDING       V                                                    
095100     BLOCK CONTAINS 0.                                                    
095200     SKIP2                                                                
095300 01  IMP-PT3                  PIC X(80).                                  
095400     EJECT                                                                
095500*01  RID-POST-PT3  -COPY W461RIDN   -L.                                   
095600     SKIP2                                                                
095700*01  RIE-POST-PT3  -COPY W461RIEN   -L.                                   
095800     SKIP2                                                                
095900*01  RIH-POST-PT3  -COPY W461RIHN   -L.                                   
096000     SKIP2                                                                
096100*01  RIO-POST-PT3  -COPY W461RIO2 -PRE SOFT-  -L.                         
096200     SKIP2                                                                
096300*01  RKB-POST-PT3  -COPY W461RKBN -L.                                     
096400     SKIP2                                                                
096500*01  RKC-POST-PT3  -COPY W461RKCN -L.                                     
096600     SKIP2                                                                
096700*01  RKD-POST-PT3  -COPY W461RKDN -L.                                     
096801 FD  W46126                                                               
096901     RECORDING       V                                                    
097001     BLOCK CONTAINS 0.                                                    
097101     SKIP2                                                                
097201 01  IMP-IN                   PIC X(80).                                  
097301     EJECT                                                                
097401*01  RID-POST-IN  -COPY W461RIDN   -L.                                    
097501     SKIP2                                                                
097601*01  RIE-POST-IN  -COPY W461RIEN   -L.                                    
097701     SKIP2                                                                
097801*01  RIH-POST-IN  -COPY W461RIHN   -L.                                    
097901     SKIP2                                                                
098001*01  RIO-POST-IN  -COPY W461RIO2 -PRE SOFT-  -L.                          
098101     SKIP2                                                                
098201*01  RKB-POST-IN  -COPY W461RKBN   -L.                                    
098301     SKIP2                                                                
098401*01  RKC-POST-IN  -COPY W461RKCN   -L.                                    
098501     SKIP2                                                                
098601*01  RKD-POST-IN  -COPY W461RKDN   -L.                                    
098701 FD  W46122                                                               
098801     RECORDING       V                                                    
098901     BLOCK CONTAINS 0.                                                    
099001     SKIP2                                                                
099101 01  IMP-CZ                   PIC X(80).                                  
099201     EJECT                                                                
099301*01  RID-POST-CZ  -COPY W461RIDN   -L.                                    
099401     SKIP2                                                                
099501*01  RIE-POST-CZ  -COPY W461RIEN   -L.                                    
099601     SKIP2                                                                
099701*01  RIH-POST-CZ  -COPY W461RIHN   -L.                                    
099801     SKIP2                                                                
099901*01  RIO-POST-CZ  -COPY W461RIO2 -PRE SOFT-  -L.                          
100001     SKIP2                                                                
100101*01  RKB-POST-CZ  -COPY W461RKBN   -L.                                    
100201     SKIP2                                                                
100301*01  RKC-POST-CZ  -COPY W461RKCN   -L.                                    
100401     SKIP2                                                                
100501*01  RKD-POST-CZ  -COPY W461RKDN   -L.                                    
100601 FD  W46123                                                               
100701     RECORDING       V                                                    
100801     BLOCK CONTAINS 0.                                                    
100901     SKIP2                                                                
101001 01  IMP-HU                   PIC X(80).                                  
101101     EJECT                                                                
101201*01  RID-POST-HU  -COPY W461RIDN   -L.                                    
101301     SKIP2                                                                
101401*01  RIE-POST-HU  -COPY W461RIEN   -L.                                    
101501     SKIP2                                                                
101601*01  RIH-POST-HU  -COPY W461RIHN   -L.                                    
101701     SKIP2                                                                
101801*01  RIO-POST-HU  -COPY W461RIO2 -PRE SOFT-  -L.                          
101901     SKIP2                                                                
102001*01  RKB-POST-HU  -COPY W461RKBN   -L.                                    
102101     SKIP2                                                                
102201*01  RKC-POST-HU  -COPY W461RKCN   -L.                                    
102301     SKIP2                                                                
102401*01  RKD-POST-HU  -COPY W461RKDN   -L.                                    
102501     EJECT                                                                
102600 SD  SORTFIL                                                              
102700                .                                                         
102800*01  FILLER -COPY W461S014.                                               
102900     EJECT                                                                
103000*01  FILLER -COPY W461S009.                                               
103100     EJECT                                                                
103200*01  FILLER -COPY W461S001.                                               
103300     EJECT                                                                
103400*01  FILLER -COPY W461S002.                                               
103500     EJECT                                                                
103600*01  FILLER -COPY W461S003.                                               
103700     EJECT                                                                
103800*01  FILLER -COPY W461S004.                                               
103900     EJECT                                                                
104000*01  FILLER -COPY W461S005.                                               
104100     EJECT                                                                
104200*01  FILLER -COPY W461S006.                                               
104300     EJECT                                                                
104400*01  FILLER -COPY W461S007.                                               
104500     EJECT                                                                
104600*01  FILLER -COPY W461S008.                                               
104700     EJECT                                                                
104800*01  FILLER -COPY W461S010.                                               
104900     EJECT                                                                
105000*01  FILLER -COPY W461S011.                                               
105100     EJECT                                                                
105200*01  FILLER -COPY W461S012.                                               
105300     EJECT                                                                
105400*01  FILLER -COPY W461S013.                                               
105500     EJECT                                                                
105600*01  FILLER -COPY W461S015.                                               
105700     EJECT                                                                
105800*01  FILLER -COPY W461S016.                                               
105900     EJECT                                                                
106000*01  FILLER -COPY W461S020.                                               
106100     EJECT                                                                
106200*01  FILLER -COPY W461S021.                                               
106300     EJECT                                                                
106400*01  FILLER -COPY W461S022.                                               
106500     EJECT                                                                
106600*01  FILLER -COPY W461S023 -PRE SORT- .                                   
106700     EJECT                                                                
106800*01  FILLER -COPY W461S024 -PRE SORT- .                                   
106900     EJECT                                                                
107000*01  FILLER -COPY W461S025 -PRE SORT- .                                   
107100     EJECT                                                                
107200*01  FILLER -COPY W37183.                                                 
107300     EJECT                                                                
107400*01  FILLER -COPY W461S051.                                               
107500     EJECT                                                                
107600*01  FILLER -COPY W37119.                                                 
107700     EJECT                                                                
107800*01  FILLER -COPY W37167.                                                 
107900     EJECT                                                                
108000 WORKING-STORAGE SECTION.                                                 
108100                                                                          
108200*    -- CHECKED BY WY2000                                                 
108300*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
108400 77  IDPGM                       PIC X(8)    VALUE 'W4612000'.            
108500     SKIP2                                                                
108600*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
108700                                                                          
108800 77  JA                          PIC X(1)    VALUE 'J'.                   
108900 77  NEJ                         PIC X(1)    VALUE 'N'.                   
109000 77  YES                         PIC X(1)    VALUE 'Y'.                   
109100                                                                          
109200*- - - - - - - - - - - - - -  TVÅ-STÄLLIGA ISO-KODER                      
109300*01  -COPY W460LISO                                                       
109400     EJECT                                                                
109500*- - - - - - - - - - - - - -  SWITCHAR                                    
109600 77  FL-GODKEND-KDFAKTYP         PIC X(1).                                
109700 77  LAGRAD-KDLIDEL              PIC S9(1)   VALUE +9.                    
109800 77  W-IDLOPNRE                  PIC S9(3).                               
109900     SKIP2                                                                
110000*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
110100                                                                          
110200 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
110300     SKIP2                                                                
110400 01  W-VKORDBTO-ORDER-LB         PIC S9(8)V9(1) COMP-3 VALUE ZERO.        
110500 01  SPAR-IDPTYP                 PIC X(3)  VALUE SPACE.                   
110600 01  SPAR-IDORDNR                PIC S9(7) COMP-3.                        
110700 01  SPAR-BEVOLREF               PIC X(10).                               
110800 01  SPAR-IDDC                   PIC X(2).                                
110900*      --- VALID IDDC CODES                                               
111000*                                                                         
111100*01    -COPY WWDC99                                                       
111200       EJECT                                                              
111300                                                                          
111400 01  WS-KDVALUTA                 PIC 9(3)  VALUE ZERO.                    
111500 01  SPAR-RIM-IDORDNR            PIC 9(7)  VALUE ZERO.                    
111600 01  SPAR-RIM-IDKUNDNR           PIC 9(6)  VALUE ZERO.                    
111700     SKIP2                                                                
111800 01  SPAR-RIL-IDPTYP             PIC X(3)  VALUE SPACE.                   
111900 01  SPAR-RIL-PRAVDRAG           PIC 9(7)V9(2)  VALUE ZERO.               
112000 01  SPAR-RIL-PREMBHNT           PIC 9(7)V9(2)  VALUE ZERO.               
112100 01  SPAR-RIL-PRFOERS            PIC 9(7)V9(2)  VALUE ZERO.               
112200 01  SPAR-RIL-PRFRAKT            PIC 9(7)V9(2)  VALUE ZERO.               
112300 01  SPAR-RIL-PRLEGKST           PIC 9(7)V9(2)  VALUE ZERO.               
112400 01  SPAR-RIL-PRMOMS             PIC 9(7)V9(2)  VALUE ZERO.               
112500 01  SPAR-RIL-SUFKTTILL          PIC 9(7)V9(2)  VALUE ZERO.               
112600 01  SPAR-PRFRAKT-LOC            PIC 9(7)V9(2)  VALUE ZERO.               
112700 01  SPAR-IDTRPTNR               PIC 9(3)       VALUE ZERO.               
112800 01  SPAR-IDLBBET                PIC X(12)      VALUE SPACE.              
112900                                                                          
113000 01  WS-TIAAMMDD                 PIC 9(6).                                
113100 01  WS-DELAD-TIAAMMDD  REDEFINES WS-TIAAMMDD.                            
113200     03  FILLER                  PIC 9(2).                                
113300     03  WS-TIMM                 PIC 9(2).                                
113400     03  WS-TIDD                 PIC 9(2).                                
113500     EJECT                                                                
113600 01  TEST-IDDISTR                PIC 9(5) COMP-3.                         
113700*01  FILLER -COPY WWDIS100 -RED TEST-IDDISTR.                             
113800     SKIP3                                                                
113900*01  FILLER -COPY WWDIS130 -RED TEST-IDDISTR.                             
114000     SKIP3                                                                
114100*01  FILLER -COPY WWDIS102 -RED TEST-IDDISTR.                             
114200     SKIP3                                                                
114300*01  FILLER -COPY WWDIS121 -RED TEST-IDDISTR.                             
114400     SKIP3                                                                
114500 01  DYNAMISKA-SUBPROGRAM.                                                
114600   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
114700   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
114800   03  W460DIS1                  PIC X(8)    VALUE 'W460DIS1'.            
114900   03  W400ARTU                  PIC X(8)    VALUE 'W400ARTU'.            
115000     SKIP3                                                                
115100*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
115200                                                                          
115300 01  RETURKODER.                                                          
115400   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
115500   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
115600   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
115700     EJECT                                                                
115800*- - - - - - - - - - - - - -  INDEX ETC.                                  
115900     SKIP2                                                                
116000 01  IX-STATNR             PIC S9(3) COMP-3.                              
116100 01  IX-RAD                PIC S9(5) COMP-3.                              
116200 01  IX-KOLUMN             PIC S9(5) COMP-3.                              
116300 01  KDHBLKRV              PIC S9(5) COMP-3.                              
116400     EJECT                                                                
116500*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
116600                                                                          
116700*01  -COPY W0005       -PRE  POSTSUM-.                                    
116800     EJECT                                                                
116900*- - - - - - - - - - - - - -  PARAMETRAR TILL W460DIS1                    
117000                                                                          
117100*01  -COPY W460DIS1                                                       
117200     EJECT                                                                
117300*- - - - - - - - - - - - - -  PARAMETRAR TILL W440ARTU                    
117400                                                                          
117500*01  -COPY W400ARTU                                                       
117600     EJECT                                                                
117700*    --- PARAMETRAR TILL COPYTEXT   WWOMVAND                              
117800*01 -COPY WWOMVAND                                                        
117900     EJECT                                                                
118000*    --- PARAMETRAR TILL W930VAL                                          
118100*01 -COPY W930VAL                                                         
118200     EJECT                                                                
118300******************************************************************        
118400*         SORTERADE POSTER                                       *        
118500******************************************************************        
118600 01  WSORT-AREA.                                                          
118700   03  WSORT-AREA-X          PIC X(200).                                  
118800     SKIP3                                                                
118900*  03  FILLER -COPY W461S014   -PRE WSORT- -RED WSORT-AREA-X.             
119000     EJECT                                                                
119100*  03  FILLER -COPY W461S009   -PRE WSORT- -RED WSORT-AREA-X.             
119200     EJECT                                                                
119300*  03  FILLER -COPY W461S001   -PRE WSORT- -RED WSORT-AREA-X.             
119400     EJECT                                                                
119500*  03  FILLER -COPY W461S002   -PRE WSORT- -RED WSORT-AREA-X.             
119600     EJECT                                                                
119700*  03  FILLER -COPY W461S003   -PRE WSORT- -RED WSORT-AREA-X.             
119800     EJECT                                                                
119900*  03  FILLER -COPY W461S004   -PRE WSORT- -RED WSORT-AREA-X.             
120000     EJECT                                                                
120100*  03  FILLER -COPY W461S005   -PRE WSORT- -RED WSORT-AREA-X.             
120200     EJECT                                                                
120300*  03  FILLER -COPY W461S006   -PRE WSORT- -RED WSORT-AREA-X.             
120400     EJECT                                                                
120500*  03  FILLER -COPY W461S007   -PRE WSORT- -RED WSORT-AREA-X.             
120600     EJECT                                                                
120700*  03  FILLER -COPY W461S008   -PRE WSORT- -RED WSORT-AREA-X.             
120800     EJECT                                                                
120900*  03  FILLER -COPY W461S010   -PRE WSORT- -RED WSORT-AREA-X.             
121000     EJECT                                                                
121100*  03  FILLER -COPY W461S011   -PRE WSORT- -RED WSORT-AREA-X.             
121200     EJECT                                                                
121300*  03  FILLER -COPY W461S012   -PRE WSORT- -RED WSORT-AREA-X.             
121400     EJECT                                                                
121500*  03  FILLER -COPY W461S013   -PRE WSORT- -RED WSORT-AREA-X.             
121600     EJECT                                                                
121700*  03  FILLER -COPY W461S015   -PRE WSORT- -RED WSORT-AREA-X.             
121800     EJECT                                                                
121900*  03  FILLER -COPY W461S016   -PRE WSORT- -RED WSORT-AREA-X.             
122000     EJECT                                                                
122100*  03  FILLER -COPY W461S020   -PRE WSORT- -RED WSORT-AREA-X.             
122200     EJECT                                                                
122300*  03  FILLER -COPY W461S021   -PRE WSORT- -RED WSORT-AREA-X.             
122400     EJECT                                                                
122500*  03  FILLER -COPY W461S022   -PRE WSORT- -RED WSORT-AREA-X.             
122600     EJECT                                                                
122700*  03  FILLER -COPY W461S023   -PRE WSORT- -RED WSORT-AREA-X.             
122800     EJECT                                                                
122900*  03  FILLER -COPY W461S024   -PRE WSORT- -RED WSORT-AREA-X.             
123000     EJECT                                                                
123100*  03  FILLER -COPY W461S025   -PRE WSORT- -RED WSORT-AREA-X.             
123200     EJECT                                                                
123300*  03  FILLER -COPY W37183     -PRE WSORT- -RED WSORT-AREA-X.             
123400     EJECT                                                                
123500*  03  FILLER -COPY W461S051   -PRE WSORT- -RED WSORT-AREA-X.             
123600     EJECT                                                                
123700*  03  FILLER -COPY W37119 -PRE WSORT-RKG- -RED WSORT-AREA-X.             
123800     EJECT                                                                
123900*  03  FILLER -COPY W37167 -PRE WSORT-RKO- -RED WSORT-AREA-X.             
124000     EJECT                                                                
124100*  03  FILLER -COPY W461S040   -PRE WSORT- -RED WSORT-AREA-X.             
124200     EJECT                                                                
124300*  03  FILLER -COPY W461S041   -PRE WSORT- -RED WSORT-AREA-X.             
124400     EJECT                                                                
124500*  03  FILLER -COPY W461S042   -PRE WSORT- -RED WSORT-AREA-X.             
124600     EJECT                                                                
124700*  03  FILLER -COPY W461S043   -PRE WSORT- -RED WSORT-AREA-X.             
124800     EJECT                                                                
124900******************************************************************        
125000*         W46120-AREA                                            *        
125100******************************************************************        
125200 01  W46120-AREA.                                                         
125300   03  W46120-AREA-X         PIC X(200).                                  
125400     SKIP3                                                                
125500*  03  FILLER -COPY W461S002   -PRE W46120- -RED W46120-AREA-X.           
125600     EJECT                                                                
125700*  03  FILLER -COPY W461S003   -PRE W46120- -RED W46120-AREA-X.           
125800     EJECT                                                                
125900*  03  FILLER -COPY W461S004   -PRE W46120- -RED W46120-AREA-X.           
126000     EJECT                                                                
126100*  03  FILLER -COPY W461S005   -PRE W46120- -RED W46120-AREA-X.           
126200     EJECT                                                                
126300*  03  FILLER -COPY W461S006   -PRE W46120- -RED W46120-AREA-X.           
126400     EJECT                                                                
126500*  03  FILLER -COPY W461S007   -PRE W46120- -RED W46120-AREA-X.           
126600     EJECT                                                                
126700*  03  FILLER -COPY W461S008   -PRE W46120- -RED W46120-AREA-X.           
126800     EJECT                                                                
126900******************************************************************        
127000*    POSTER TILL IMPORTÖR, KORTFORMAT.                           *        
127100******************************************************************        
127200     SKIP2                                                                
127300 01  UTKORT.                                                              
127401   03  UTKORT-X             PIC X(200).                                   
127500     SKIP3                                                                
127600*  03 FILLER -COPY W461RIAN   -RED UTKORT-X.                              
127700     EJECT                                                                
127800*  03 FILLER -COPY W461RIBN   -RED UTKORT-X.                              
127900     EJECT                                                                
128000*  03 FILLER -COPY W461RICN   -RED UTKORT-X.                              
128100     EJECT                                                                
128200*  03 FILLER -COPY W461RIFN   -RED UTKORT-X.                              
128300     EJECT                                                                
128400*  03 FILLER -COPY W461RIGN   -RED UTKORT-X.                              
128500     EJECT                                                                
128600*  03 FILLER -COPY W461RIIN   -RED UTKORT-X.                              
128700     EJECT                                                                
128800*  03 FILLER -COPY W461RIJN   -RED UTKORT-X.                              
128900     EJECT                                                                
129000*  03 FILLER -COPY W461RIK1   -RED UTKORT-X.                              
129100     EJECT                                                                
129200*  03 FILLER -COPY W461RILN   -RED UTKORT-X.                              
129300     EJECT                                                                
129400*  03 FILLER -COPY W461RIM2   -RED UTKORT-X.                              
129500     EJECT                                                                
129600*  03 FILLER -COPY W461RINN   -RED UTKORT-X.                              
129700     EJECT                                                                
129800*  03 FILLER -COPY W461RIPN   -RED UTKORT-X.                              
129900     EJECT                                                                
130000*  03 FILLER -COPY W461RIQN   -RED UTKORT-X.                              
130100     EJECT                                                                
130200*  03 FILLER -COPY W461RIRN   -RED UTKORT-X.                              
130300     EJECT                                                                
130400*  03 FILLER -COPY W461RITN   -RED UTKORT-X.                              
130500     EJECT                                                                
130600*  03 FILLER -COPY W461RIUN   -RED UTKORT-X.                              
130700     EJECT                                                                
130800*  03 FILLER -COPY W461RIWN   -RED UTKORT-X.                              
130900     EJECT                                                                
131000*  03 FILLER -COPY W461RIXN   -RED UTKORT-X.                              
131100     EJECT                                                                
131200*  03 FILLER -COPY W461RIYN   -RED UTKORT-X.                              
131300     EJECT                                                                
131400*  03 FILLER -COPY W461RIZN   -RED UTKORT-X.                              
131500     EJECT                                                                
131600*  03 FILLER -COPY W461RKEN   -RED UTKORT-X.                              
131700     EJECT                                                                
131800*  03 FILLER -COPY W461RKFN   -RED UTKORT-X.                              
131900     EJECT                                                                
132000*  03 FILLER -COPY W461RKGN   -RED UTKORT-X.                              
132100     EJECT                                                                
132200*  03 FILLER -COPY W461RKHN   -RED UTKORT-X.                              
132300     EJECT                                                                
132400*  03 FILLER -COPY W461RKJN   -RED UTKORT-X.                              
132500     EJECT                                                                
132600*  03 FILLER -COPY W461RKON   -PRE RKO- -RED UTKORT-X.                    
132700     EJECT                                                                
132800*                                                                         
132900 01  UTKORT-LONG.                                                         
133000     SKIP3                                                                
133100   03  UTKORT-LONGX    -COPY W461RIO2 -L.                                 
133200     SKIP3                                                                
133300*  03 FILLER -COPY W461RIDN   -RED UTKORT-LONGX.                          
133400     EJECT                                                                
133500*  03 FILLER -COPY W461RIEN   -RED UTKORT-LONGX.                          
133600     EJECT                                                                
133700*  03 FILLER -COPY W461RIHN   -RED UTKORT-LONGX.                          
133800     EJECT                                                                
133900*  03 FILLER -COPY W461RIO1   -RED UTKORT-LONGX.                          
134000     EJECT                                                                
134100*  03 FILLER -COPY W461RIO2   -PRE SOFT- -RED UTKORT-LONGX.               
134201     EJECT                                                                
134300*  03 FILLER -COPY W461RKBN   -RED UTKORT-LONGX.                          
134400     EJECT                                                                
134500*  03 FILLER -COPY W461RKCN   -RED UTKORT-LONGX.                          
134600     EJECT                                                                
134700*  03 FILLER -COPY W461RKDN   -RED UTKORT-LONGX.                          
134800     EJECT                                                                
134900 PROCEDURE DIVISION.                                                      
135000     SKIP2                                                                
135100     PERFORM A-INIT                                                       
135200                                                                          
135300     SORT SORTFIL ASCENDING                                               
135400                  EMB-SOR0-IDPTYP                                         
135500                  EMB-SOR0-IDLOPNR                                        
135600                  DUPLICATES IN ORDER                                     
135700          USING   INFIL                                                   
135800          OUTPUT PROCEDURE B-BEARBETNING                                  
135900     SKIP2                                                                
136000     IF SORT-RETURN > ZERO                                                
136100       DISPLAY '***  W4612000  - FEL VID SORTERING ' SORT-RETURN          
136200       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
136300     ELSE                                                                 
136400       PERFORM Z-FINIT                                                    
136500       MOVE ZERO TO RETURN-CODE                                           
136600       GOBACK                                                             
136700                                                                          
136800     END-IF                                                               
136900     .                                                                    
137000     EJECT                                                                
137100 A-INIT SECTION.                                                          
137200     SKIP2                                                                
137300     OPEN OUTPUT W46120                                                   
137400     W46127 W46129                                                        
137500     W46130 W4612Z                                                        
137600     W46138                                                               
137700     W46171 W46161 W46179 W46168                                          
137800     W46178 W46176                                                        
137900     W46173 W46170 W46140 W46139                                          
138000     W46174 W46131 W46175 W46172                                          
138100     W46180 W46134 W4612A W4612H                                          
138200     W46128 W4612B W4612C                                                 
138300     W4612D W4612E W4612F W4612G                                          
138400     W4612I W4612J W4612K W4612L W4612M W4612N                            
138500     W4612P W461CN W461ZA W461CN1                                         
138600     W461PT3                                                              
138701     W46126                                                               
138801     W46122 W46123                                                        
138900     MOVE SPACE TO UTKORT                                                 
139000     MOVE SPACE TO W46120-AREA                                            
139100     .                                                                    
139200     EJECT                                                                
139300 B-BEARBETNING SECTION.                                                   
139400     SKIP2                                                                
139500     PERFORM S01-LAS-SORTERAD-INFIL                                       
139600     PERFORM UNTIL SORTFIL-EOF = JA                                       
139700       IF WSORT-BIP-IDPTYP = '001'                                        
139800         PERFORM BA-BIPACKNING                                            
139900       ELSE                                                               
140000         EVALUATE TRUE                                                    
140100         WHEN WSORT-BIP-IDPTYP = '002'                                    
140200           PERFORM BB-ORDERBEKR-HUVUD                                     
140300         WHEN WSORT-BIP-IDPTYP = '003'                                    
140400           PERFORM BC-ORDERBEKR-ENTYDIG-ERS                               
140500         WHEN WSORT-BIP-IDPTYP = '004'                                    
140600           PERFORM BD-ORDERBEKR-EJ-ENTYDIG-ERS                            
140700         WHEN WSORT-BIP-IDPTYP = '005'                                    
140800           PERFORM BE-ORDERBEKR-TEXT                                      
140900         WHEN WSORT-BIP-IDPTYP = '006'                                    
141000           PERFORM BF-ORDERBEKR-KVANT-ANPASS                              
141100         WHEN WSORT-BIP-IDPTYP = '007'                                    
141200           PERFORM BG-ORDERBEKR-LAGER-AVBOK                               
141300         WHEN WSORT-BIP-IDPTYP = '008'                                    
141400           PERFORM BH-ORDERBEKR-STOPPADE-RADER                            
141500         WHEN WSORT-BIP-IDPTYP = '009'                                    
141600           PERFORM BI-SERVGRAD                                            
141700         WHEN WSORT-BIP-IDPTYP = '010'                                    
141800           PERFORM BJ-FAKTURA-HUVUD-1                                     
141900           PERFORM BK-FAKTURA-HUVUD-2                                     
142000         WHEN WSORT-BIP-IDPTYP = '011'                                    
142100           PERFORM BL-FAKTURA-REFERENS                                    
142200         WHEN WSORT-BIP-IDPTYP = '012'                                    
142300           PERFORM BM-FAKTURA-KOLLI                                       
142400         WHEN WSORT-BIP-IDPTYP = '013'                                    
142500           PERFORM BN-FAKTURA-RAD-1                                       
142600         WHEN WSORT-BIP-IDPTYP = '014'                                    
142700           PERFORM BO-EMBALLAGE-PROFORMA                                  
142800         WHEN WSORT-BIP-IDPTYP = '051'                                    
142900           PERFORM BQ-RENSNING                                            
143000         WHEN WSORT-BIP-IDPTYP = '015'                                    
143100           PERFORM BR-BYPASS-ORDER                                        
143200         WHEN WSORT-BIP-IDPTYP = '016'                                    
143300           PERFORM BS-AVST-SALDON                                         
143400         WHEN WSORT-BIP-IDPTYP = '020'                                    
143500           PERFORM BT-ON-ORDER                                            
143600         WHEN WSORT-BIP-IDPTYP = '021'                                    
143700           PERFORM BU-HUV-KRED                                            
143800         WHEN WSORT-BIP-IDPTYP = '022'                                    
143900           PERFORM BV-RAD-KRED                                            
144000         WHEN WSORT-BIP-IDPTYP = '023'                                    
144100           PERFORM BX-RAD-KRED                                            
144200         WHEN WSORT-BIP-IDPTYP = '024'                                    
144300           PERFORM BY-RAD-KRED                                            
144400         WHEN WSORT-BIP-IDPTYP = 'RKF'                                    
144500           PERFORM BZ-RAD-KVITT                                           
144600         WHEN WSORT-BIP-IDPTYP = 'RKG'                                    
144700           PERFORM B1-RAD-BYTES                                           
144800         WHEN WSORT-BIP-IDPTYP = 'RKO'                                    
144900           PERFORM B3-RAD-POINT                                           
145000         WHEN WSORT-BIP-IDPTYP = '025'                                    
145100           PERFORM B2-RAD-KRED-PMN                                        
145200         END-EVALUATE                                                     
145300       END-IF                                                             
145400       MOVE WSORT-BIP-IDPTYP TO SPAR-IDPTYP                               
145500       PERFORM S01-LAS-SORTERAD-INFIL                                     
145600     END-PERFORM                                                          
145700     .                                                                    
145800 BA-BIPACKNING SECTION.                                                   
145900     SKIP2                                                                
146000     IF WSORT-BIP-KDFAKTYP = 'R' OR 'G' OR 'K'                            
146100       MOVE 'RIA'                 TO RIA-IDPTYP                           
146200       MOVE WSORT-BIP-IDDC        TO RIA-IDDC                             
146300       MOVE WSORT-BIP-IDDISTR     TO RIA-IDDISTR                          
146400       MOVE WSORT-BIP-IDKUNDNR    TO RIA-IDKUNDNR                         
146500       MOVE WSORT-BIP-IDORDNR     TO RIA-IDORDNR                          
146600       MOVE WSORT-BIP-KDORDKL     TO RIA-KDORDKL                          
146700       MOVE WSORT-BIP-IDARTNR     TO RIA-IDARTNR                          
146800       MOVE WSORT-BIP-REKSIFFR    TO RIA-REKSIFFR                         
146900       MOVE WSORT-BIP-BERADREF    TO RIA-BERADREF                         
147000       MOVE WSORT-BIP-IDRONR      TO RIA-IDRONR                           
147100       MOVE WSORT-BIP-BEVOLREF    TO RIA-BEVOLREF                         
147200       MOVE WSORT-BIP-KVLEVART    TO RIA-KVLEVART                         
147300       MOVE WSORT-BIP-KDRESTR     TO RIA-KDRESTR                          
147400                                                                          
147500       MOVE WSORT-BIP-TIAAMMDD    TO WS-TIAAMMDD                          
147600       MOVE WS-TIMM               TO RIA-TIMM                             
147700       MOVE WS-TIDD               TO RIA-TIDD                             
147800       MOVE WSORT-BIP-TIKLOCK     TO RIA-TIKLOCK                          
147900                                                                          
148000       PERFORM S10-SPLITTA-IMPORTORER                                     
148100     END-IF                                                               
148200     MOVE SPACE TO UTKORT                                                 
148300     .                                                                    
148400     EJECT                                                                
148500 BB-ORDERBEKR-HUVUD SECTION.                                              
148600     SKIP2                                                                
148700     MOVE WSORT-OBHUV-W461002    TO W46120-OBHUV-W461002                  
148800     PERFORM S20-SKRIV-OBHUV                                              
148900     MOVE NEJ TO FL-GODKEND-KDFAKTYP                                      
149000     MOVE 0  TO W-IDLOPNRE                                                
149100     IF WSORT-OBHUV-KDFAKTYP = 'R' OR 'G' OR 'K'                          
149200       MOVE JA TO FL-GODKEND-KDFAKTYP                                     
149300       MOVE +9 TO LAGRAD-KDLIDEL                                          
149400       MOVE 'RIB'                 TO RIB-IDPTYP                           
149500       MOVE WSORT-OBHUV-IDDISTR   TO RIB-IDDISTR                          
149600       MOVE WSORT-OBHUV-IDKUNDNR  TO RIB-IDKUNDNR                         
149700       MOVE WSORT-OBHUV-IDORDNR   TO RIB-IDORDNR                          
149800       SPAR-IDORDNR                                                       
149900       MOVE WSORT-OBHUV-KDFRAKT   TO RIB-KDFRAKT                          
150000       MOVE WSORT-OBHUV-BEVOLREF  TO RIB-BEVOLREF                         
150100       SPAR-BEVOLREF                                                      
150200       MOVE WSORT-OBHUV-BEVARREF  TO RIB-BEVARREF                         
150300       MOVE WSORT-OBHUV-KDORDKL   TO RIB-KDORDKL                          
150400       MOVE WSORT-OBHUV-TIORDREG  TO RIB-TIORDREG                         
150500       PERFORM S10-SPLITTA-IMPORTORER                                     
150600     END-IF                                                               
150700     MOVE SPACE TO UTKORT                                                 
150800     .                                                                    
150900     EJECT                                                                
151000 BC-ORDERBEKR-ENTYDIG-ERS SECTION.                                        
151100     SKIP2                                                                
151200     MOVE WSORT-OBEN-W461003     TO W46120-OBEN-W461003                   
151300     PERFORM S21-SKRIV-OBEN                                               
151400     IF FL-GODKEND-KDFAKTYP = JA                                          
151500       IF WSORT-OBEN-KDLIDEL NOT = LAGRAD-KDLIDEL                         
151600         MOVE 'RIC'                 TO RIC-IDPTYP                         
151700         MOVE WSORT-OBEN-IDDISTR    TO RIC-IDDISTR                        
151800         MOVE WSORT-OBEN-IDKUNDNR   TO RIC-IDKUNDNR                       
151900         MOVE WSORT-OBEN-IDORDNR    TO RIC-IDORDNR                        
152000         MOVE WSORT-OBEN-KDLIDEL    TO RIC-KDLIDEL                        
152100         LAGRAD-KDLIDEL                                                   
152200         PERFORM S10-SPLITTA-IMPORTORER                                   
152300         MOVE SPACE TO UTKORT                                             
152400       END-IF                                                             
152500       IF W-IDLOPNRE NOT = WSORT-OBEN-IDLOPNRE                            
152600       AND WSORT-OBEN-KDRO = 2                                            
152700         MOVE 'RIU'                 TO RIU-IDPTYP                         
152800         MOVE WSORT-OBEN-IDDC       TO RIU-IDDC                           
152900         MOVE WSORT-OBEN-IDARTNR    TO RIU-IDARTNR                        
153000         MOVE WSORT-OBEN-REKSIFFR   TO RIU-REKSIFFR                       
153100         MOVE WSORT-OBEN-IDLOPNRE   TO RIU-IDLOPNRE                       
153200         MOVE ZERO                  TO RIU-IDKORTNR                       
153300         MOVE 2                     TO RIU-KDRO                           
153400                                                                          
153500         PERFORM S10-SPLITTA-IMPORTORER                                   
153600         MOVE SPACE TO UTKORT                                             
153700         MOVE WSORT-OBEN-IDLOPNRE TO W-IDLOPNRE                           
153800       END-IF                                                             
153900       MOVE 'RID'                 TO RID-IDPTYP                           
154000       MOVE WSORT-OBEN-IDDC       TO RID-IDDC                             
154100       MOVE WSORT-OBEN-IDARTNR    TO RID-IDARTNR                          
154200       MOVE WSORT-OBEN-REKSIFFR   TO RID-REKSIFFR                         
154300       MOVE WSORT-OBEN-IDLOPNRE   TO RID-IDLOPNRE                         
154400       MOVE WSORT-OBEN-IDKORTNR   TO RID-IDKORTNR                         
154500       MOVE WSORT-OBEN-BERADREF   TO RID-BERADREF                         
154600       IF WSORT-OBEN-IDRONR > +0                                          
154700         MOVE WSORT-OBEN-IDRONR     TO RID-IDRONR                         
154800         MOVE WSORT-OBEN-BEVOLREF   TO RID-BEVOLREF                       
154900       ELSE                                                               
155000         MOVE SPAR-IDORDNR    TO RID-IDRONR                               
155100         MOVE SPAR-BEVOLREF   TO RID-BEVOLREF                             
155200       END-IF                                                             
155300       MOVE WSORT-OBEN-KDRESTR    TO RID-KDRESTR                          
155400       MOVE WSORT-OBEN-KDERS      TO RID-KDERS                            
155500       MOVE WSORT-OBEN-KVBEART    TO RID-KVBEART                          
155600       MOVE WSORT-OBEN-IDARTNR-TILLK TO RID-IDARTNR-TILLK                 
155700       MOVE WSORT-OBEN-REKSIFFR-TILLK TO RID-REKSIFFR-TILLK               
155800       MOVE WSORT-OBEN-KVBEART-TILLK TO RID-KVBEART-TILLK                 
155900       MOVE WSORT-OBEN-DIERS-KVOT TO RID-DIERS-KVOT                       
156000       MOVE WSORT-OBEN-KDERSUP    TO RID-KDERSUP                          
156100       MOVE WSORT-OBEN-KDDSP      TO RID-KDDSP                            
156200       PERFORM S10-SPLITTA-IMPORTORER                                     
156300     END-IF                                                               
156400     MOVE SPACE TO UTKORT-LONG                                            
156500     .                                                                    
156600     EJECT                                                                
156700 BD-ORDERBEKR-EJ-ENTYDIG-ERS SECTION.                                     
156800     SKIP2                                                                
156900     MOVE WSORT-OBEEN-W461004    TO W46120-OBEEN-W461004                  
157000     PERFORM S22-SKRIV-OBEEN                                              
157100     IF FL-GODKEND-KDFAKTYP = JA                                          
157200       IF WSORT-OBEEN-KDLIDEL NOT = LAGRAD-KDLIDEL                        
157300         MOVE 'RIC'                 TO RIC-IDPTYP                         
157400         MOVE WSORT-OBEEN-IDDISTR   TO RIC-IDDISTR                        
157500         MOVE WSORT-OBEEN-IDKUNDNR  TO RIC-IDKUNDNR                       
157600         MOVE WSORT-OBEEN-IDORDNR   TO RIC-IDORDNR                        
157700         MOVE WSORT-OBEEN-KDLIDEL   TO RIC-KDLIDEL                        
157800         LAGRAD-KDLIDEL                                                   
157900         PERFORM S10-SPLITTA-IMPORTORER                                   
158000         MOVE SPACE TO UTKORT                                             
158100       END-IF                                                             
158200       MOVE 'RIE'                  TO RIE-IDPTYP                          
158300       MOVE WSORT-OBEEN-IDDC       TO RIE-IDDC                            
158400       MOVE WSORT-OBEEN-IDARTNR    TO RIE-IDARTNR                         
158500       MOVE WSORT-OBEEN-REKSIFFR   TO RIE-REKSIFFR                        
158600       MOVE WSORT-OBEEN-IDLOPNRE   TO RIE-IDLOPNRE                        
158700       MOVE WSORT-OBEEN-IDKORTNR   TO RIE-IDKORTNR                        
158800       MOVE WSORT-OBEEN-BERADREF   TO RIE-BERADREF                        
158900       IF WSORT-OBEEN-IDRONR > +0                                         
159000         MOVE WSORT-OBEEN-IDRONR    TO RIE-IDRONR                         
159100         MOVE WSORT-OBEEN-BEVOLREF  TO RIE-BEVOLREF                       
159200       ELSE                                                               
159300         MOVE SPAR-IDORDNR    TO RIE-IDRONR                               
159400         MOVE SPAR-BEVOLREF   TO RIE-BEVOLREF                             
159500       END-IF                                                             
159600       MOVE WSORT-OBEEN-KDRESTR   TO RIE-KDRESTR                          
159700       MOVE WSORT-OBEEN-KDERS     TO RIE-KDERS                            
159800       MOVE WSORT-OBEEN-KVBEART   TO RIE-KVBEART                          
159900       MOVE WSORT-OBEEN-IDARTNR-TILLK TO RIE-IDARTNR-TILLK                
160000       MOVE WSORT-OBEEN-REKSIFFR-TILLK TO RIE-REKSIFFR-TILLK              
160100       MOVE WSORT-OBEEN-KVBEART-TILLK TO RIE-KVBEART-TILLK                
160200       MOVE WSORT-OBEEN-DIERS-KVOT TO RIE-DIERS-KVOT                      
160300       MOVE WSORT-OBEEN-KDERSUP   TO RIE-KDERSUP                          
160400       MOVE WSORT-OBEEN-KDDSP     TO RIE-KDDSP                            
160500       PERFORM S10-SPLITTA-IMPORTORER                                     
160600     END-IF                                                               
160700     MOVE SPACE TO UTKORT-LONG                                            
160800     .                                                                    
160900     EJECT                                                                
161000 BE-ORDERBEKR-TEXT SECTION.                                               
161100     SKIP2                                                                
161200     MOVE WSORT-OBTEXT-W461005   TO W46120-OBTEXT-W461005                 
161300     PERFORM S23-SKRIV-OBTEXT                                             
161400     IF FL-GODKEND-KDFAKTYP = JA                                          
161500       IF WSORT-OBTEXT-KDLIDEL NOT = LAGRAD-KDLIDEL                       
161600         MOVE 'RIC'                 TO RIC-IDPTYP                         
161700         MOVE WSORT-OBTEXT-IDDISTR  TO RIC-IDDISTR                        
161800         MOVE WSORT-OBTEXT-IDKUNDNR TO RIC-IDKUNDNR                       
161900         MOVE WSORT-OBTEXT-IDORDNR  TO RIC-IDORDNR                        
162000         MOVE WSORT-OBTEXT-KDLIDEL  TO RIC-KDLIDEL                        
162100         LAGRAD-KDLIDEL                                                   
162200         PERFORM S10-SPLITTA-IMPORTORER                                   
162300         MOVE SPACE TO UTKORT                                             
162400       END-IF                                                             
162500       IF W-IDLOPNRE NOT = WSORT-OBTEXT-IDLOPNRE                          
162600       AND WSORT-OBTEXT-KDRO = 2                                          
162700         MOVE 'RIU'                   TO RIU-IDPTYP                       
162800         MOVE WSORT-OBTEXT-IDDC       TO RIU-IDDC                         
162900         MOVE WSORT-OBTEXT-IDARTNR    TO RIU-IDARTNR                      
163000         MOVE WSORT-OBTEXT-REKSIFFR   TO RIU-REKSIFFR                     
163100         MOVE WSORT-OBTEXT-IDLOPNRE   TO RIU-IDLOPNRE                     
163200         MOVE ZERO                    TO RIU-IDKORTNR                     
163300         MOVE 2                       TO RIU-KDRO                         
163400                                                                          
163500         MOVE WSORT-OBTEXT-IDLOPNRE   TO W-IDLOPNRE                       
163600         PERFORM S10-SPLITTA-IMPORTORER                                   
163700         MOVE SPACE TO UTKORT                                             
163800       END-IF                                                             
163900       MOVE 'RIF'                   TO RIF-IDPTYP                         
164000       MOVE WSORT-OBTEXT-IDDC       TO RIF-IDDC                           
164100       MOVE WSORT-OBTEXT-IDARTNR    TO RIF-IDARTNR                        
164200       MOVE WSORT-OBTEXT-REKSIFFR   TO RIF-REKSIFFR                       
164300       MOVE WSORT-OBTEXT-IDLOPNRE   TO RIF-IDLOPNRE                       
164400       MOVE WSORT-OBTEXT-IDKORTNR   TO RIF-IDKORTNR                       
164500       MOVE WSORT-OBTEXT-BERADREF   TO RIF-BERADREF                       
164600       IF WSORT-OBTEXT-IDRONR > +0                                        
164700         MOVE WSORT-OBTEXT-IDRONR   TO RIF-IDRONR                         
164800         MOVE WSORT-OBTEXT-BEVOLREF TO RIF-BEVOLREF                       
164900       ELSE                                                               
165000         MOVE SPAR-IDORDNR    TO RIF-IDRONR                               
165100         MOVE SPAR-BEVOLREF   TO RIF-BEVOLREF                             
165200       END-IF                                                             
165300       MOVE WSORT-OBTEXT-KDRESTR  TO RIF-KDRESTR                          
165400       MOVE WSORT-OBTEXT-KDERS    TO RIF-KDERS                            
165500       MOVE WSORT-OBTEXT-KVBEART  TO RIF-KVBEART                          
165600       MOVE WSORT-OBTEXT-BEERS    TO RIF-BEERS                            
165700       MOVE WSORT-OBTEXT-KDERSUP  TO RIF-KDERSUP                          
165800       MOVE WSORT-OBTEXT-KDDSP    TO RIF-KDDSP                            
165900       PERFORM S10-SPLITTA-IMPORTORER                                     
166000     END-IF                                                               
166100     MOVE SPACE TO UTKORT                                                 
166200     .                                                                    
166300     EJECT                                                                
166400 BF-ORDERBEKR-KVANT-ANPASS SECTION.                                       
166500     SKIP2                                                                
166600     MOVE WSORT-OBKVAN-W461006   TO W46120-OBKVAN-W461006                 
166700     PERFORM S24-SKRIV-OBKVAN                                             
166800     IF FL-GODKEND-KDFAKTYP = JA                                          
166900       IF WSORT-OBKVAN-KDLIDEL NOT = LAGRAD-KDLIDEL                       
167000         MOVE 'RIC'                 TO RIC-IDPTYP                         
167100         MOVE WSORT-OBKVAN-IDDISTR  TO RIC-IDDISTR                        
167200         MOVE WSORT-OBKVAN-IDKUNDNR TO RIC-IDKUNDNR                       
167300         MOVE WSORT-OBKVAN-IDORDNR  TO RIC-IDORDNR                        
167400         MOVE WSORT-OBKVAN-KDLIDEL  TO RIC-KDLIDEL                        
167500         LAGRAD-KDLIDEL                                                   
167600         PERFORM S10-SPLITTA-IMPORTORER                                   
167700         MOVE SPACE TO UTKORT                                             
167800       END-IF                                                             
167900       MOVE 'RIG'                   TO RIG-IDPTYP                         
168000       MOVE WSORT-OBKVAN-IDDC       TO RIG-IDDC                           
168100       MOVE WSORT-OBKVAN-IDARTNR    TO RIG-IDARTNR                        
168200       MOVE WSORT-OBKVAN-REKSIFFR   TO RIG-REKSIFFR                       
168300       MOVE WSORT-OBKVAN-BERADREF   TO RIG-BERADREF                       
168400       IF WSORT-OBKVAN-IDRONR > +0                                        
168500         MOVE WSORT-OBKVAN-IDRONR   TO RIG-IDRONR                         
168600         MOVE WSORT-OBKVAN-BEVOLREF TO RIG-BEVOLREF                       
168700       ELSE                                                               
168800         MOVE SPAR-IDORDNR    TO RIG-IDRONR                               
168900         MOVE SPAR-BEVOLREF   TO RIG-BEVOLREF                             
169000       END-IF                                                             
169100       MOVE WSORT-OBKVAN-KDRESTR  TO RIG-KDRESTR                          
169200       MOVE WSORT-OBKVAN-KVBEART  TO RIG-KVBEART                          
169300       MOVE WSORT-OBKVAN-KVBEART-Q TO RIG-KVBEART-Q                       
169400       MOVE WSORT-OBKVAN-KVQPACK-1 TO RIG-KVQPACK-1                       
169500       MOVE WSORT-OBKVAN-KDDSP     TO RIG-KDDSP                           
169600                                                                          
169700       MOVE WSORT-OBKVAN-TIAAMMDD TO WS-TIAAMMDD                          
169800       MOVE WS-TIMM               TO RIG-TIMM                             
169900       MOVE WS-TIDD               TO RIG-TIDD                             
170000       MOVE WSORT-OBKVAN-TIKLOCK  TO RIG-TIKLOCK                          
170100                                                                          
170200       PERFORM S10-SPLITTA-IMPORTORER                                     
170300     END-IF                                                               
170400     MOVE SPACE TO UTKORT                                                 
170500     .                                                                    
170600     EJECT                                                                
170700 BG-ORDERBEKR-LAGER-AVBOK SECTION.                                        
170800     SKIP2                                                                
170900     MOVE WSORT-OBLAG-W461007    TO W46120-OBLAG-W461007                  
171000     PERFORM S25-SKRIV-OBLAG                                              
171100     IF FL-GODKEND-KDFAKTYP = JA                                          
171200       IF WSORT-OBLAG-KDLIDEL NOT = LAGRAD-KDLIDEL                        
171300         MOVE 'RIC'                 TO RIC-IDPTYP                         
171400         MOVE WSORT-OBLAG-IDDISTR   TO RIC-IDDISTR                        
171500         MOVE WSORT-OBLAG-IDKUNDNR  TO RIC-IDKUNDNR                       
171600         MOVE WSORT-OBLAG-IDORDNR   TO RIC-IDORDNR                        
171700         MOVE WSORT-OBLAG-KDLIDEL   TO RIC-KDLIDEL                        
171800         LAGRAD-KDLIDEL                                                   
171900         PERFORM S10-SPLITTA-IMPORTORER                                   
172000         MOVE SPACE TO UTKORT                                             
172100       END-IF                                                             
172200       MOVE 'RIH'                  TO RIH-IDPTYP                          
172300       MOVE WSORT-OBLAG-IDDC       TO RIH-IDDC                            
172400       MOVE WSORT-OBLAG-IDARTNR    TO RIH-IDARTNR                         
172500       MOVE WSORT-OBLAG-REKSIFFR   TO RIH-REKSIFFR                        
172600       MOVE WSORT-OBLAG-BERADREF   TO RIH-BERADREF                        
172700       IF WSORT-OBLAG-IDRONR > +0                                         
172800         MOVE WSORT-OBLAG-IDRONR   TO RIH-IDRONR                          
172900         MOVE WSORT-OBLAG-BEVOLREF TO RIH-BEVOLREF                        
173000       ELSE                                                               
173100         MOVE SPAR-IDORDNR    TO RIH-IDRONR                               
173200         MOVE SPAR-BEVOLREF   TO RIH-BEVOLREF                             
173300       END-IF                                                             
173400       MOVE WSORT-OBLAG-KDRESTR   TO RIH-KDRESTR                          
173500       MOVE WSORT-OBLAG-KVBEART   TO RIH-KVBEART                          
173600       MOVE WSORT-OBLAG-KVAVBART  TO RIH-KVAVBART                         
173700       MOVE WSORT-OBLAG-KVRO      TO RIH-KVRO                             
173800       MOVE WSORT-OBLAG-KDDSP     TO RIH-KDDSP                            
173900       MOVE WSORT-OBLAG-TIDISPIN  TO RIH-TIDISPIN                         
174000                                                                          
174100       MOVE WSORT-OBLAG-TIAAMMDD  TO WS-TIAAMMDD                          
174200       MOVE WS-TIMM               TO RIH-TIMM                             
174300       MOVE WS-TIDD               TO RIH-TIDD                             
174400       MOVE WSORT-OBLAG-TIKLOCK   TO RIH-TIKLOCK                          
174500                                                                          
174600       PERFORM S10-SPLITTA-IMPORTORER                                     
174700     END-IF                                                               
174800     MOVE SPACE TO UTKORT-LONG                                            
174900     .                                                                    
175000     EJECT                                                                
175100 BH-ORDERBEKR-STOPPADE-RADER SECTION.                                     
175200     SKIP2                                                                
175300     MOVE WSORT-OBSTOP-W461008   TO W46120-OBSTOP-W461008                 
175400     PERFORM S26-SKRIV-OBSTOP                                             
175500     IF FL-GODKEND-KDFAKTYP = JA                                          
175600       IF WSORT-OBSTOP-KDLIDEL NOT = LAGRAD-KDLIDEL                       
175700         MOVE 'RIC'                 TO RIC-IDPTYP                         
175800         MOVE WSORT-OBSTOP-IDDISTR  TO RIC-IDDISTR                        
175900         MOVE WSORT-OBSTOP-IDKUNDNR TO RIC-IDKUNDNR                       
176000         MOVE WSORT-OBSTOP-IDORDNR  TO RIC-IDORDNR                        
176100         MOVE WSORT-OBSTOP-KDLIDEL  TO RIC-KDLIDEL                        
176200         LAGRAD-KDLIDEL                                                   
176300         PERFORM S10-SPLITTA-IMPORTORER                                   
176400         MOVE SPACE TO UTKORT                                             
176500       END-IF                                                             
176600       MOVE 'RII'                   TO RII-IDPTYP                         
176700       MOVE WSORT-OBSTOP-IDDC       TO RII-IDDC                           
176800       MOVE WSORT-OBSTOP-IDARTNR    TO RII-IDARTNR                        
176900       MOVE WSORT-OBSTOP-REKSIFFR   TO RII-REKSIFFR                       
177000       MOVE WSORT-OBSTOP-BERADREF   TO RII-BERADREF                       
177100       IF WSORT-OBSTOP-IDRONR > +0                                        
177200         MOVE WSORT-OBSTOP-IDRONR  TO RII-IDRONR                          
177300         MOVE WSORT-OBSTOP-BEVOLREF TO RII-BEVOLREF                       
177400       ELSE                                                               
177500         MOVE SPAR-IDORDNR    TO RII-IDRONR                               
177600         MOVE SPAR-BEVOLREF   TO RII-BEVOLREF                             
177700       END-IF                                                             
177800       MOVE WSORT-OBSTOP-KDRESTR  TO RII-KDRESTR                          
177900       MOVE WSORT-OBSTOP-KVBEART  TO RII-KVBEART                          
178000                                                                          
178100       MOVE WSORT-OBSTOP-TIAAMMDD TO WS-TIAAMMDD                          
178200       MOVE WS-TIMM               TO RII-TIMM                             
178300       MOVE WS-TIDD               TO RII-TIDD                             
178400       MOVE WSORT-OBSTOP-TIKLOCK  TO RII-TIKLOCK                          
178500                                                                          
178600*      MOVE WSORT-OBSTOP-KDDSP    TO RII-KDDSP                            
178700*****FIX FÖR ATT EJ FÅ KDDSP = 0                                          
178800       IF WSORT-OBSTOP-KDDSP = +0                                         
178900         MOVE +1 TO RII-KDDSP                                             
179000       ELSE                                                               
179100         MOVE WSORT-OBSTOP-KDDSP TO RII-KDDSP                             
179200       END-IF                                                             
179300       PERFORM S10-SPLITTA-IMPORTORER                                     
179400     END-IF                                                               
179500     MOVE SPACE TO UTKORT                                                 
179600     .                                                                    
179700     EJECT                                                                
179800 BI-SERVGRAD SECTION.                                                     
179900     SKIP2                                                                
180000     IF WSORT-SERV-KDFAKTYP = 'R' OR 'G' OR 'K'                           
180100       MOVE 'RIJ'                 TO RIJ-IDPTYP                           
180200       MOVE WSORT-SERV-IDDC       TO RIJ-IDDC                             
180300       MOVE WSORT-SERV-IDDISTR    TO RIJ-IDDISTR                          
180400       MOVE WSORT-SERV-IDKUNDNR   TO RIJ-IDKUNDNR                         
180500       MOVE WSORT-SERV-KDORDKL    TO RIJ-KDORDKL                          
180600       MOVE WSORT-SERV-IDORDNR    TO RIJ-IDORDNR                          
180700       MOVE WSORT-SERV-IDARTNR    TO RIJ-IDARTNR                          
180800       MOVE WSORT-SERV-REKSIFFR   TO RIJ-REKSIFFR                         
180900       MOVE WSORT-SERV-KVBEART    TO RIJ-KVBEART                          
181000       PERFORM S10-SPLITTA-IMPORTORER                                     
181100     END-IF                                                               
181200     MOVE SPACE TO UTKORT                                                 
181300     .                                                                    
181400     EJECT                                                                
181500 BJ-FAKTURA-HUVUD-1 SECTION.                                              
181600     SKIP2                                                                
181700     MOVE NEJ TO FL-GODKEND-KDFAKTYP                                      
181800     IF WSORT-FHUV-KDFAKTYP = 'R' OR 'G' OR 'K'                           
181900       MOVE JA TO FL-GODKEND-KDFAKTYP                                     
182000       MOVE 'RIK'                 TO RIK-IDPTYP                           
182100       MOVE WSORT-FHUV-IDDISTR    TO RIK-IDDISTR                          
182200       MOVE WSORT-FHUV-KDFAKTYP   TO RIK-KDFAKTYP                         
182300       MOVE WSORT-FHUV-IDFAKT     TO RIK-IDFAKT                           
182400       MOVE WSORT-FHUV-TIFAKT     TO RIK-TIFAKT                           
182500       MOVE WSORT-FHUV-IDDC       TO RIK-IDDC                             
182600                                     SPAR-IDDC                            
182700       MOVE WSORT-FHUV-IDFRASED   TO RIK-IDFRASED                         
182800       MOVE WSORT-FHUV-SUFKTBEL   TO RIK-SUFKTBEL                         
182900                                                                          
183000       PERFORM BJA-BYT-KDVALISO-TILL-KDVALUTA                             
183100                                                                          
183200       MOVE WS-KDVALUTA           TO RIK-KDVALUTA                         
183300       MOVE WSORT-FHUV-PRKURS     TO RIK-PRKURS                           
183400       MOVE WSORT-FHUV-SUFKTUTL   TO RIK-SUFKTUTL                         
183500       MOVE WSORT-FHUV-KDFAKNOT   TO RIK-KDFAKNOT                         
183600       PERFORM S10-SPLITTA-IMPORTORER                                     
183700     END-IF                                                               
183800     MOVE SPACE TO UTKORT                                                 
183900     .                                                                    
184000     EJECT                                                                
184100 BJA-BYT-KDVALISO-TILL-KDVALUTA SECTION.                                  
184200                                                                          
184300     MOVE ZERO                      TO WS-KDVALUTA                        
184400     MOVE +1                        TO TAB-IX                             
184500     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
184600       IF WSORT-FHUV-KDVALISO = TAB-KDVALISO (TAB-IX)                     
184700         MOVE TAB-KDVALUTA (TAB-IX) TO WS-KDVALUTA                        
184800         MOVE TAB-IX-MAX            TO TAB-IX                             
184900       END-IF                                                             
185000       ADD +1 TO TAB-IX                                                   
185100     END-PERFORM                                                          
185200     .                                                                    
185300     EJECT                                                                
185400                                                                          
185500 BK-FAKTURA-HUVUD-2 SECTION.                                              
185600     SKIP2                                                                
185700     IF FL-GODKEND-KDFAKTYP = JA                                          
185800                                                                          
185900       MOVE WSORT-FHUV-SOR0-IDDISTR TO TEST-IDDISTR                       
186000       IF DIS102-ITALIEN                                                  
186100         MOVE 'RIL'                TO SPAR-RIL-IDPTYP                     
186200         MOVE WSORT-FHUV-PRAVDRAG  TO SPAR-RIL-PRAVDRAG                   
186300         MOVE WSORT-FHUV-PREMBHNT  TO SPAR-RIL-PREMBHNT                   
186400         MOVE WSORT-FHUV-PRFOERS   TO SPAR-RIL-PRFOERS                    
186500         MOVE WSORT-FHUV-PRFRAKT   TO SPAR-RIL-PRFRAKT                    
186600         MOVE WSORT-FHUV-PRFRAKT-LOC TO SPAR-PRFRAKT-LOC                  
186700         MOVE WSORT-FHUV-IDTRPTNR  TO SPAR-IDTRPTNR                       
186800         MOVE WSORT-FHUV-IDLBBET   TO SPAR-IDLBBET                        
186900         MOVE WSORT-FHUV-PRLEGKST  TO SPAR-RIL-PRLEGKST                   
187000         MOVE WSORT-FHUV-PRMOMS    TO SPAR-RIL-PRMOMS                     
187100         MOVE WSORT-FHUV-SUFKTTILL TO SPAR-RIL-SUFKTTILL                  
187200       ELSE                                                               
187300         MOVE 'RIL'                TO RIL-IDPTYP                          
187400         MOVE WSORT-FHUV-PRAVDRAG  TO RIL-PRAVDRAG                        
187500         MOVE WSORT-FHUV-PREMBHNT  TO RIL-PREMBHNT                        
187600         MOVE WSORT-FHUV-PRFOERS   TO RIL-PRFOERS                         
187700         MOVE WSORT-FHUV-PRFRAKT   TO RIL-PRFRAKT                         
187800         MOVE WSORT-FHUV-PRFRAKT-LOC TO SPAR-PRFRAKT-LOC                  
187900         MOVE WSORT-FHUV-IDTRPTNR  TO SPAR-IDTRPTNR                       
188000         MOVE WSORT-FHUV-IDLBBET   TO SPAR-IDLBBET                        
188100         MOVE WSORT-FHUV-PRLEGKST  TO RIL-PRLEGKST                        
188200         MOVE WSORT-FHUV-PRMOMS    TO RIL-PRMOMS                          
188300         MOVE WSORT-FHUV-SUFKTTILL TO RIL-SUFKTTILL                       
188400         MOVE WSORT-FHUV-KDFRAKT   TO RIL-KDFRAKT                         
188500         PERFORM S10-SPLITTA-IMPORTORER                                   
188600       END-IF                                                             
188700     END-IF                                                               
188800     MOVE SPACE TO UTKORT                                                 
188900     .                                                                    
189000     EJECT                                                                
189100 BL-FAKTURA-REFERENS SECTION.                                             
189200** DET SKAPAS TVÅ POSTTYP '011' PÅ ORDER SOM HAR BLANDAT                  
189300** VANLIGA ARTIKLAR OCH DIREKLEVERANSARTIKLAR. DET SKA                    
189400** BARA SKAPAS EN 'RIM'-POST TILL VIPS                                    
189500     SKIP2                                                                
189600     IF SPAR-RIL-IDPTYP = 'RIL'                                           
189700         MOVE 'RIL'                TO RIL-IDPTYP                          
189800         MOVE SPAR-RIL-PRAVDRAG    TO RIL-PRAVDRAG                        
189900         MOVE SPAR-RIL-PREMBHNT    TO RIL-PREMBHNT                        
190000         MOVE SPAR-RIL-PRFOERS     TO RIL-PRFOERS                         
190100         MOVE SPAR-RIL-PRFRAKT     TO RIL-PRFRAKT                         
190200         MOVE SPAR-RIL-PRLEGKST    TO RIL-PRLEGKST                        
190300         MOVE SPAR-RIL-PRMOMS      TO RIL-PRMOMS                          
190400         MOVE SPAR-RIL-SUFKTTILL   TO RIL-SUFKTTILL                       
190500         MOVE WSORT-FREF-KDFRAKT   TO RIL-KDFRAKT                         
190600         PERFORM S10-SPLITTA-IMPORTORER                                   
190700         MOVE SPACE TO SPAR-RIL-IDPTYP                                    
190800         MOVE SPACE TO UTKORT                                             
190900     END-IF                                                               
191000                                                                          
191100     IF FL-GODKEND-KDFAKTYP = JA                                          
191200        IF WSORT-FREF-IDKUNDNR    = SPAR-RIM-IDKUNDNR                     
191300           AND WSORT-FREF-IDORDNR = SPAR-RIM-IDORDNR                      
191400           AND SPAR-IDPTYP        = '011'                                 
191500           MOVE WSORT-FREF-IDKUNDNR   TO SPAR-RIM-IDKUNDNR                
191600           MOVE WSORT-FREF-IDORDNR    TO SPAR-RIM-IDORDNR                 
191700           CONTINUE                                                       
191800        ELSE                                                              
191900           MOVE 'RIM'                 TO RIM-IDPTYP                       
192000           MOVE WSORT-FREF-IDKUNDNR   TO RIM-IDKUNDNR                     
192100           MOVE WSORT-FREF-IDPRODNR   TO RIM-IDPRODNR                     
192200           MOVE WSORT-FREF-IDORDNR    TO RIM-IDORDNR                      
192300           MOVE WSORT-FREF-TIORDREG   TO RIM-TIORDREG                     
192400           MOVE WSORT-FREF-KDORDKL    TO RIM-KDORDKL                      
192500           MOVE WSORT-FREF-BEKUNDRF   TO RIM-BEKUNDRF                     
192600           MOVE WSORT-FREF-BEVARREF   TO RIM-BEVARREF                     
192700           MOVE WSORT-FREF-KDREFNOT   TO RIM-KDREFNOT                     
192800           MOVE WSORT-FREF-KDFRAKT    TO RIM-KDFRAKT                      
192900           MOVE WSORT-FREF-IDTRPBOT   TO RIM-IDTRPBOT                     
193000           MOVE WSORT-FREF-IDTRPBON   TO RIM-IDTRPBON                     
193100           MOVE SPAR-IDDC             TO WS-IDDC                          
193200           IF     NDC-US                                                  
193300              AND (WSORT-FREF-KDORDKL = 0 OR 1)                           
193400              AND SPAR-IDTRPTNR      = 901                                
193500**            AND SPAR-IDLBBET       = 'FEDXPO'                           
193600             IF SPAR-PRFRAKT-LOC > ZERO                                   
193700               MOVE SPAR-PRFRAKT-LOC  TO RIM-PRFRAKT-LOC                  
193800             ELSE                                                         
193900               PERFORM BLA-BERAKNA-PRFRAKT-LOC                            
194000             END-IF                                                       
194100           ELSE                                                           
194200             MOVE SPAR-PRFRAKT-LOC    TO RIM-PRFRAKT-LOC                  
194300           END-IF                                                         
194400           PERFORM S10-SPLITTA-IMPORTORER                                 
194500           MOVE RIM-IDKUNDNR          TO SPAR-RIM-IDKUNDNR                
194600           MOVE RIM-IDORDNR           TO SPAR-RIM-IDORDNR                 
194700        END-IF                                                            
194800     END-IF                                                               
194900     MOVE SPACE TO UTKORT                                                 
195000     .                                                                    
195100     EJECT                                                                
195200 BLA-BERAKNA-PRFRAKT-LOC SECTION.                                         
195300     SKIP2                                                                
195400     MOVE ZERO                      TO RIM-PRFRAKT-LOC                    
195500     COMPUTE W-VKORDBTO-ORDER-LB ROUNDED =                                
195600             WSORT-FREF-VKORDBTO-ORDER   *                                
195700             CONV-KG-TO-LB                                                
195800                                                                          
195900     EVALUATE W-VKORDBTO-ORDER-LB                                         
196000       WHEN  0.1 THRU  2.9                                                
196100         MOVE 8.50                  TO RIM-PRFRAKT-LOC                    
196200       WHEN  3.0 THRU  5.9                                                
196300         MOVE 11.15                 TO RIM-PRFRAKT-LOC                    
196400       WHEN  6.0 THRU  9.9                                                
196500         MOVE 15.00                 TO RIM-PRFRAKT-LOC                    
196600       WHEN 10.0 THRU 15.9                                                
196700         MOVE 20.00                 TO RIM-PRFRAKT-LOC                    
196800       WHEN 16.0 THRU 20.9                                                
196900         MOVE 24.15                 TO RIM-PRFRAKT-LOC                    
197000       WHEN 21.0 THRU 25.9                                                
197100         MOVE 28.40                 TO RIM-PRFRAKT-LOC                    
197200       WHEN 26.0 THRU 30.9                                                
197300         MOVE 32.70                 TO RIM-PRFRAKT-LOC                    
197400       WHEN 31.0 THRU 40.9                                                
197500         MOVE 40.50                 TO RIM-PRFRAKT-LOC                    
197600       WHEN 41.0 THRU 50.9                                                
197700         MOVE 47.75                 TO RIM-PRFRAKT-LOC                    
197800       WHEN 51.0 THRU 60.9                                                
197900         MOVE 56.10                 TO RIM-PRFRAKT-LOC                    
198000       WHEN 61.0 THRU 70.9                                                
198100         MOVE 66.20                 TO RIM-PRFRAKT-LOC                    
198200       WHEN 71.0 THRU 80.9                                                
198300         MOVE 76.40                 TO RIM-PRFRAKT-LOC                    
198400       WHEN 81.0 THRU 90.9                                                
198500         MOVE 86.35                 TO RIM-PRFRAKT-LOC                    
198600       WHEN 91.0 THRU 99.9                                                
198700         MOVE 95.05                 TO RIM-PRFRAKT-LOC                    
198800       WHEN 100.0 THRU 110.9                                              
198900         MOVE 105.00                TO RIM-PRFRAKT-LOC                    
199000       WHEN 111.0 THRU 120.9                                              
199100         MOVE 114.50                TO RIM-PRFRAKT-LOC                    
199200       WHEN 121.0 THRU 130.9                                              
199300         MOVE 124.00                TO RIM-PRFRAKT-LOC                    
199400       WHEN 131.0 THRU 140.9                                              
199500         MOVE 133.60                TO RIM-PRFRAKT-LOC                    
199600       WHEN 141.0 THRU 150.9                                              
199700         MOVE 143.15                TO RIM-PRFRAKT-LOC                    
199800       WHEN OTHER                                                         
199900         MOVE SPAR-PRFRAKT-LOC      TO RIM-PRFRAKT-LOC                    
200000     END-EVALUATE                                                         
200100                                                                          
200200     .                                                                    
200300     EJECT                                                                
200400 BM-FAKTURA-KOLLI SECTION.                                                
200500     SKIP2                                                                
200600     IF FL-GODKEND-KDFAKTYP = JA                                          
200700       MOVE 'RIN'                   TO RIN-IDPTYP                         
200800       MOVE WSORT-FKOLLI-IDKUNDNR   TO RIN-IDKUNDNR                       
200900       MOVE WSORT-FKOLLI-IDPRODNR   TO RIN-IDPRODNR                       
201000       MOVE WSORT-FKOLLI-IDKOLLI    TO RIN-IDKOLLI                        
201100       MOVE WSORT-FKOLLI-IDORDNR    TO RIN-IDORDNR                        
201200       MOVE WSORT-FKOLLI-VKORDBTO-KOLLI                                   
201300                                    TO RIN-VKORDBTO-KOLLI                 
201400       MOVE WSORT-FKOLLI-VLORDBTO-KOLLI                                   
201500                                    TO RIN-VLORDBTO-KOLLI                 
201600       MOVE WSORT-FKOLLI-IDLBBET    TO RIN-IDLBBET                        
201700       MOVE WSORT-FKOLLI-KDEMBTYP   TO RIN-KDEMBTYP                       
201800       MOVE WSORT-FKOLLI-IDFAKT-GNB TO RIN-IDFAKT-GNB                     
201900       PERFORM S10-SPLITTA-IMPORTORER                                     
202000     END-IF                                                               
202100     MOVE SPACE TO UTKORT                                                 
202200     .                                                                    
202300     EJECT                                                                
202400 BN-FAKTURA-RAD-1 SECTION.                                                
202500     SKIP2                                                                
202600     IF FL-GODKEND-KDFAKTYP = JA                                          
202700                                                                          
202800       MOVE BIP-SOR0-IDDISTR TO DIS1-IDDISTR                              
202900                                TEST-IDDISTR                              
203000       CALL W460DIS1 USING DIS1-W460DIS1                                  
203100       IF DIS1-IDLANDX2 = ISO-SAUDI     OR                                
203200          DIS1-IDLANDX2 = ISO-PERU      OR                                
203300         (DIS1-IDLANDX2 = ISO-BRASILIEN AND DIS121-BRASIL)                
203400         MOVE 'RIO'               TO RIO-IDPTYP                           
203500         MOVE WSORT-FRAD-IDARTNR  TO RIO-IDARTNR                          
203600         MOVE WSORT-FRAD-IDORDNR  TO RIO-IDORDNR                          
203700         MOVE WSORT-FRAD-REKSIFFR TO RIO-REKSIFFR                         
203800         MOVE WSORT-FRAD-BERADREF TO RIO-BERADREF                         
203900         MOVE WSORT-FRAD-KVBEART  TO RIO-KVBEART                          
204000         MOVE WSORT-FRAD-KVLEVART TO RIO-KVLEVART                         
204100         MOVE WSORT-FRAD-RESERVG  TO RIO-RESERVG                          
204200         MOVE WSORT-FRAD-PRARTBTO-EXP TO RIO-PRARTBTO-EXP                 
204300         MOVE WSORT-FRAD-PRARTNTO TO RIO-PRARTNTO                         
204400         MOVE WSORT-FRAD-IDFKNGRP TO RIO-IDFKNGRP                         
204500         MOVE WSORT-FRAD-KDPRODSL TO RIO-KDPRODSL                         
204600         MOVE WSORT-FRAD-KDDSP    TO RIO-KDDSP                            
204700         MOVE WSORT-FRAD-KDVVKL   TO RIO-KDVVKL                           
204800         MOVE WSORT-FRAD-KDVRINFO TO RIO-KDVRINFO                         
204900         MOVE WSORT-FRAD-FLINVEST TO RIO-FLINVEST                         
205000         MOVE WSORT-FRAD-FLPRTILL TO RIO-FLPRTILL                         
205100         MOVE WSORT-FRAD-FLDIRLEV TO RIO-FLDIRLEV                         
205200         MOVE WSORT-FRAD-KDARTRAB TO RIO-KDRABATT                         
205300         MOVE SPAR-IDDC           TO RIO-IDDC                             
205400         MOVE WSORT-FRAD-KDPSLLOC TO RIO-KDPSLLOC                         
205500         MOVE WSORT-FRAD-PRAVCOST TO RIO-PRAVCOST                         
205600         MOVE WSORT-FRAD-PRAVCOST-CORE TO RIO-PRAVCOST-CORE               
205700         PERFORM S10-SPLITTA-IMPORTORER                                   
205800         MOVE SPACE TO UTKORT-LONG                                        
205900       ELSE                                                               
206000         MOVE 'RIO'               TO SOFT-RIO-IDPTYP                      
206100         MOVE WSORT-FRAD-IDARTNR  TO SOFT-RIO-IDARTNR                     
206200         MOVE WSORT-FRAD-IDORDNR  TO SOFT-RIO-IDORDNR                     
206300         MOVE WSORT-FRAD-REKSIFFR TO SOFT-RIO-REKSIFFR                    
206400         MOVE WSORT-FRAD-BERADREF TO SOFT-RIO-BERADREF                    
206500         MOVE WSORT-FRAD-KVBEART  TO SOFT-RIO-KVBEART                     
206600         MOVE WSORT-FRAD-KVLEVART TO SOFT-RIO-KVLEVART                    
206700         MOVE WSORT-FRAD-RESERVG  TO SOFT-RIO-RESERVG                     
206800         MOVE WSORT-FRAD-PRARTBTO-EXP TO SOFT-RIO-PRARTBTO-EXP            
206900         MOVE WSORT-FRAD-PRARTNTO TO SOFT-RIO-PRARTNTO                    
207000         MOVE WSORT-FRAD-IDFKNGRP TO SOFT-RIO-IDFKNGRP                    
207100         MOVE WSORT-FRAD-KDPRODSL TO SOFT-RIO-KDPRODSL                    
207200         MOVE WSORT-FRAD-KDDSP    TO SOFT-RIO-KDDSP                       
207300         MOVE WSORT-FRAD-KDVVKL   TO SOFT-RIO-KDVVKL                      
207400         MOVE WSORT-FRAD-KDVRINFO TO SOFT-RIO-KDVRINFO                    
207500         MOVE WSORT-FRAD-FLINVEST TO SOFT-RIO-FLINVEST                    
207600         MOVE WSORT-FRAD-FLPRTILL TO SOFT-RIO-FLPRTILL                    
207700         MOVE WSORT-FRAD-FLDIRLEV TO SOFT-RIO-FLDIRLEV                    
207800         MOVE WSORT-FRAD-KDARTRAB TO SOFT-RIO-KDRABATT                    
207900* SVERIGE VIPS KLARAR INTE ALPHA DC - TAS FRÅN HUVUD ISTÄLLET             
208000*        MOVE WSORT-FRAD-IDDC     TO SOFT-RIO-IDDC                        
208100         MOVE SPAR-IDDC           TO SOFT-RIO-IDDC                        
208200         MOVE WSORT-FRAD-KDPSLLOC TO SOFT-RIO-KDPSLLOC                    
208300         MOVE WSORT-FRAD-PRAVCOST TO SOFT-RIO-PRAVCOST                    
208400         MOVE WSORT-FRAD-PRAVCOST-CORE TO SOFT-RIO-PRAVCOST-CORE          
208500         MOVE WSORT-FRAD-IDBIL    TO SOFT-RIO-IDBIL                       
208600         MOVE SPACE               TO SOFT-RIO-BEVOLREF                    
208701         MOVE SPACE               TO SOFT-RIO-IDVIN                       
208800         PERFORM S10-SPLITTA-IMPORTORER                                   
208900         MOVE SPACE TO UTKORT-LONG                                        
209000       END-IF                                                             
209100     END-IF                                                               
209200                                                                          
209300     IF DIS1-IDLANDX2 = ISO-DANMARK                                       
209400       PERFORM BNA-FAKTURA-RAD-DANMARK                                    
209500     END-IF                                                               
209600     IF WSORT-FRAD-IDRONR       > 0                                       
209700       PERFORM BNB-FAKTURA-RAD-2                                          
209800     END-IF                                                               
209900     .                                                                    
210000     EJECT                                                                
210100 BNA-FAKTURA-RAD-DANMARK SECTION.                                         
210200     SKIP2                                                                
210300     MOVE 'RIZ'                 TO RIZ-IDPTYP                             
210400     MOVE WSORT-FRAD-KDSRA      TO RIZ-KDSRA                              
210500     MOVE WSORT-FRAD-KDSORT     TO RIZ-KDSORT                             
210600     MOVE WSORT-FRAD-KVQPACK-1  TO RIZ-KVQPACK-1                          
210700                                                                          
210800     PERFORM BNAA-AENDRA-KDARTURS-TILL-NUM                                
210900     MOVE ARTU-KDARTURS-NUM     TO RIZ-KDARTURS                           
211000                                                                          
211100     MOVE WSORT-FRAD-IDSTATNR   TO RIZ-IDSTATNR                           
211200     MOVE WSORT-FRAD-BEART      TO RIZ-BEART                              
211300     MOVE WSORT-FRAD-VKART      TO RIZ-VKART                              
211400     PERFORM S10-SPLITTA-IMPORTORER                                       
211500     MOVE SPACE TO UTKORT                                                 
211600     .                                                                    
211700     EJECT                                                                
211800                                                                          
211900 BNAA-AENDRA-KDARTURS-TILL-NUM SECTION.                                   
212000     SKIP2                                                                
212100     MOVE WSORT-FRAD-KDARTURS    TO ARTU-KDARTURS                         
212200     MOVE SPACE                  TO ARTU-IDDC                             
212300     MOVE ZERO                   TO ARTU-IDDISTR                          
212400     CALL W400ARTU USING ARTU-W400ARTU                                    
212500     .                                                                    
212600     EJECT                                                                
212700                                                                          
212800 BNB-FAKTURA-RAD-2 SECTION.                                               
212900     SKIP2                                                                
213000     MOVE 'RIP'                  TO RIP-IDPTYP                            
213100     MOVE WSORT-FRAD-IDRONR      TO RIP-IDRONR                            
213200     IF WSORT-FRAD-TIRODAT       =  ZERO                                  
213300         MOVE YES                TO RIP-FLIHOP                            
213400      ELSE                                                                
213500         MOVE SPACE              TO RIP-FLIHOP                            
213600     END-IF                                                               
213700     MOVE WSORT-FRAD-TIORDREG    TO RIP-TIORDREG                          
213800     MOVE WSORT-FRAD-BEVOLREF    TO RIP-BEVOLREF                          
213900     MOVE WSORT-FRAD-KDORDKL-URS TO RIP-KDORDKL                           
214000     PERFORM S10-SPLITTA-IMPORTORER                                       
214100     MOVE SPACE TO UTKORT                                                 
214200     .                                                                    
214300     EJECT                                                                
214400 BO-EMBALLAGE-PROFORMA SECTION.                                           
214500     SKIP2                                                                
214600     IF WSORT-EMB-KDFAKTYP = 'R' OR 'G' OR 'K'                            
214700       MOVE 'RIQ'                 TO RIQ-IDPTYP                           
214800       MOVE WSORT-EMB-IDDISTR     TO RIQ-IDDISTR                          
214900       MOVE WSORT-EMB-IDKUNDNR    TO RIQ-IDKUNDNR                         
215000       MOVE WSORT-EMB-IDORDNR     TO RIQ-IDORDNR                          
215100       MOVE WSORT-EMB-IDDC        TO RIQ-IDDC                             
215200       MOVE WSORT-EMB-KDFAKTYP    TO RIQ-KDFAKTYP                         
215300       MOVE WSORT-EMB-IDFAKT      TO RIQ-IDFAKT                           
215400       MOVE WSORT-EMB-TIFAKT      TO RIQ-TIFAKT                           
215500       MOVE WSORT-EMB-KDPALL      TO RIQ-KDPALL                           
215600       MOVE WSORT-EMB-KVPALL      TO RIQ-KVPALL                           
215700       MOVE WSORT-EMB-KVKRAG      TO RIQ-KVKRAG                           
215800       MOVE WSORT-EMB-KVLOCK      TO RIQ-KVLOCK                           
215900       PERFORM S10-SPLITTA-IMPORTORER                                     
216000     END-IF                                                               
216100     MOVE SPACE TO UTKORT                                                 
216200     .                                                                    
216300     EJECT                                                                
216400 BQ-RENSNING SECTION.                                                     
216500     SKIP2                                                                
216600     MOVE 'RIR'                  TO RIR-IDPTYP                            
216700     MOVE WSORT-RENS-IDDC        TO RIR-IDDC                              
216800     MOVE WSORT-RENS-IDDISTR     TO RIR-IDDISTR                           
216900     MOVE WSORT-RENS-IDKUNDNR    TO RIR-IDKUNDNR                          
217000     MOVE WSORT-RENS-IDORDNR     TO RIR-IDORDNR                           
217100     MOVE WSORT-RENS-BEVOLREF    TO RIR-BEVOLREF                          
217200     MOVE WSORT-RENS-TIORDREG    TO RIR-TIORDREG                          
217300     PERFORM S10-SPLITTA-IMPORTORER                                       
217400     MOVE SPACE TO UTKORT                                                 
217500     .                                                                    
217600     EJECT                                                                
217700 BR-BYPASS-ORDER SECTION.                                                 
217800     SKIP2                                                                
217900     IF WSORT-BYPASS-KDFAKTYP = 'R' OR 'G' OR 'K'                         
218000       MOVE 'RIT'                  TO RIT-IDPTYP                          
218100       MOVE WSORT-BYPASS-IDDISTR   TO RIT-IDDISTR                         
218200       MOVE WSORT-BYPASS-IDKUNDNR  TO RIT-IDKUNDNR                        
218300       MOVE WSORT-BYPASS-IDORDNR   TO RIT-IDORDNR                         
218400       MOVE WSORT-BYPASS-KDORDKL   TO RIT-KDORDKL                         
218500       MOVE WSORT-BYPASS-IDARTNR   TO RIT-IDARTNR                         
218600       MOVE WSORT-BYPASS-REKSIFFR  TO RIT-REKSIFFR                        
218700       MOVE WSORT-BYPASS-BERADREF  TO RIT-BERADREF                        
218800       MOVE WSORT-BYPASS-BEVOLREF  TO RIT-BEVOLREF                        
218900       MOVE WSORT-BYPASS-KVBEART   TO RIT-KVBEART                         
219000       MOVE WSORT-BYPASS-KDDSP     TO RIT-KDDSP                           
219100       MOVE WSORT-BYPASS-FLABON    TO RIT-FLABON                          
219200       MOVE WSORT-BYPASS-KDTPOTYP  TO RIT-KDTPOTYP                        
219300       PERFORM S10-SPLITTA-IMPORTORER                                     
219400     END-IF                                                               
219500     MOVE SPACE TO UTKORT                                                 
219600     .                                                                    
219700     EJECT                                                                
219800 BS-AVST-SALDON SECTION.                                                  
219900     SKIP2                                                                
220000     MOVE 'RIW'                  TO RIW-IDPTYP                            
220100     MOVE WSORT-AVST-IDDISTR     TO RIW-IDDISTR                           
220200     MOVE WSORT-AVST-IDKUNDNR    TO RIW-IDKUNDNR                          
220300     MOVE WSORT-AVST-IDARTNR     TO RIW-IDARTNR                           
220400     MOVE WSORT-AVST-REKSIFFR    TO RIW-REKSIFFR                          
220500     MOVE WSORT-AVST-BEVOLREF    TO RIW-BEVOLREF                          
220600     MOVE WSORT-AVST-KVBEART     TO RIW-KVBEART                           
220700     MOVE WSORT-AVST-KVRO        TO RIW-KVRO                              
220800     PERFORM S10-SPLITTA-IMPORTORER                                       
220900     MOVE SPACE TO UTKORT                                                 
221000     .                                                                    
221100     EJECT                                                                
221200 BT-ON-ORDER SECTION.                                                     
221300     SKIP2                                                                
221400     IF WSORT-ONORD-SOR0-IDRONR > +0                                      
221500       MOVE 'RIX'                  TO RIX-IDPTYP                          
221600       MOVE WSORT-ONORD-IDDISTR     TO RIX-IDDISTR                        
221700       MOVE WSORT-ONORD-IDKUNDNR    TO RIX-IDKUNDNR                       
221800       MOVE WSORT-ONORD-IDORDNR     TO RIX-IDORDNR                        
221900       MOVE WSORT-ONORD-KDORDKL     TO RIX-KDORDKL                        
222000       MOVE WSORT-ONORD-BEVOLREF    TO RIX-BEVOLREF                       
222100       MOVE WSORT-ONORD-BEVARREF    TO RIX-BEVARREF                       
222200       MOVE WSORT-ONORD-TIORDREG    TO RIX-TIORDREG                       
222300       PERFORM S10-SPLITTA-IMPORTORER                                     
222400       MOVE SPACE TO UTKORT                                               
222500     END-IF                                                               
222600     MOVE 'RIY'                    TO RIY-IDPTYP                          
222700     MOVE WSORT-ONORD-IDDC         TO RIY-IDDC                            
222800     MOVE WSORT-ONORD-IDARTNR      TO RIY-IDARTNR                         
222900     MOVE WSORT-ONORD-REKSIFFR     TO RIY-REKSIFFR                        
223000     MOVE WSORT-ONORD-KVBEART      TO RIY-KVBEART                         
223100     MOVE WSORT-ONORD-PRARTNTO     TO RIY-PRARTNTO                        
223200     MOVE WSORT-ONORD-PRARTBTO-EXP TO RIY-PRARTBTO-EXP                    
223300     MOVE WSORT-ONORD-BERADREF     TO RIY-BERADREF                        
223400     MOVE WSORT-ONORD-KDDSP        TO RIY-KDDSP                           
223500     MOVE WSORT-ONORD-KDPRODSL     TO RIY-KDPRODSL                        
223600     MOVE WSORT-ONORD-IDFKNGRP     TO RIY-IDFKNGRP                        
223700     MOVE WSORT-ONORD-FLINVEST     TO RIY-FLINVEST                        
223800     MOVE WSORT-ONORD-KDMANPR      TO RIY-FLPRTILL                        
223900     MOVE WSORT-ONORD-FLABON       TO RIY-FLABON                          
224000     MOVE WSORT-ONORD-KDTPOTYP     TO RIY-KDTPOTYP                        
224100     PERFORM S10-SPLITTA-IMPORTORER                                       
224200     MOVE SPACE TO UTKORT                                                 
224300     .                                                                    
224400     EJECT                                                                
224500 BU-HUV-KRED SECTION.                                                     
224600     SKIP2                                                                
224700     MOVE 'RKB'                    TO RKB-IDPTYP                          
224800     MOVE WSORT-HUV-SOR0-IDDISTR   TO RKB-IDDISTR                         
224900     MOVE WSORT-HUV-SOR0-IDKUNDNR  TO RKB-IDKUNDNR                        
225000     MOVE WSORT-HUV-IDKNOTNR       TO RKB-IDKNOTNR                        
225100     MOVE WSORT-HUV-IDDC           TO RKB-IDDC                            
225200     MOVE WSORT-HUV-TIM-KN         TO RKB-TIM-KN                          
225300     MOVE WSORT-HUV-IDRAPPNR       TO RKB-IDRAPPNR                        
225400     MOVE WSORT-HUV-PREMBHNT       TO RKB-PREMBHNT                        
225500     MOVE WSORT-HUV-PRFRAKT        TO RKB-PRFRAKT                         
225600     MOVE WSORT-HUV-PRLEGKST       TO RKB-PRLEGKST                        
225700     MOVE WSORT-HUV-PRFOERS        TO RKB-PRFOERS                         
225800     MOVE WSORT-HUV-PRMOMS         TO RKB-PRMOMS                          
225900     MOVE WSORT-HUV-KDVALISO       TO RKB-KDVALISO                        
226000     MOVE WSORT-HUV-SUKRENTO       TO RKB-SUKRENTO                        
226100     MOVE WSORT-HUV-SUKRETOT       TO RKB-SUKRETOT                        
226200     PERFORM S10-SPLITTA-IMPORTORER                                       
226300     MOVE SPACE TO UTKORT-LONG                                            
226400     .                                                                    
226500     EJECT                                                                
226600 BV-RAD-KRED SECTION.                                                     
226700     SKIP2                                                                
226800     MOVE 'RKC'                    TO RKC-IDPTYP                          
226900     MOVE WSORT-RAD-IDFAKT         TO RKC-IDFAKT                          
227000     MOVE WSORT-RAD-IDORDNR        TO RKC-IDORDNR                         
227100     MOVE WSORT-RAD-IDARTNR        TO RKC-IDARTNR                         
227200     MOVE WSORT-RAD-KDANMORS       TO RKC-KDANMORS                        
227300     MOVE WSORT-RAD-KVKREANT       TO RKC-KVKREANT                        
227400     MOVE WSORT-RAD-PRARTBTO       TO RKC-PRARTBTO                        
227500     MOVE ZERO                     TO RKC-IDKOLLI                         
227600     MOVE WSORT-RAD-IDRADNR        TO RKC-IDRADNR                         
227700     MOVE WSORT-RAD-REKSIFFR       TO RKC-REKSIFFR                        
227800     MOVE WSORT-RAD-KDPSLLOC       TO RKC-KDPSLLOC                        
227900     MOVE WSORT-RAD-PRAVCOST       TO RKC-PRAVCOST                        
228000     MOVE WSORT-RAD-PRAVCOST-CORE  TO RKC-PRAVCOST-CORE                   
228100     MOVE WSORT-RAD-PRARTBTO-LOC   TO RKC-PRARTBTO-LOC                    
228200     MOVE WSORT-RAD-KDVAT          TO RKC-KDVAT                           
228300     MOVE WSORT-RAD-PRMOMS-RAD     TO RKC-PRMOMS-RAD                      
228400     MOVE WSORT-RAD-SULNELOC       TO RKC-SULNELOC                        
228500     MOVE WSORT-RAD-PRARTSTD       TO RKC-PRARTSTD                        
228600     MOVE WSORT-RAD-PRARTSJK       TO RKC-PRARTSJK                        
228701     MOVE SPACE                    TO RKC-IDTRACK                         
228801     MOVE ZERO                     TO RKC-DADATUM                         
228900     PERFORM S10-SPLITTA-IMPORTORER                                       
229000     MOVE SPACE TO UTKORT-LONG                                            
229100     .                                                                    
229200     EJECT                                                                
229300 BX-RAD-KRED SECTION.                                                     
229400     SKIP2                                                                
229500     MOVE 'RKD'                    TO RKD-IDPTYP                          
229600     MOVE WSORT-RKD-IDDISTR        TO RKD-IDDISTR                         
229700     MOVE WSORT-RKD-IDKUNDNR       TO RKD-IDKUNDNR                        
229800     MOVE WSORT-RKD-IDDC           TO RKD-IDDC                            
229900     MOVE WSORT-RKD-IDRAPPNR       TO RKD-IDRAPPNR                        
230000     MOVE WSORT-RKD-IDORDNR        TO RKD-IDORDNR                         
230100     MOVE WSORT-RKD-IDKOLLI        TO RKD-IDKOLLI                         
230200     MOVE WSORT-RKD-IDARTNR        TO RKD-IDARTNR                         
230300     MOVE WSORT-RKD-REKSIFFR       TO RKD-REKSIFFR                        
230400     MOVE WSORT-RKD-IDRADNR        TO RKD-IDRADNR                         
230500     MOVE WSORT-RKD-KDKREBEH       TO RKD-KDKREBEH                        
230600     MOVE WSORT-RKD-KDANMORS       TO RKD-KDANMORS                        
230700     MOVE WSORT-RKD-KVLEVANM       TO RKD-KVLEVANM                        
230800     MOVE WSORT-RKD-PRARTBTO       TO RKD-PRARTBTO                        
230900     MOVE WSORT-RKD-FLSKROT        TO RKD-FLSKROT                         
231000     MOVE WSORT-RKD-KDVALISO       TO RKD-KDVALISO                        
231100     MOVE WSORT-RKD-PRARTBTO-LOC   TO RKD-PRARTBTO-LOC                    
231200     MOVE WSORT-RKD-SULNELOC       TO RKD-SULNELOC                        
231300     MOVE WSORT-RKD-PRARTSTD       TO RKD-PRARTSTD                        
231400     MOVE WSORT-RKD-PRARTSJK       TO RKD-PRARTSJK                        
231500     PERFORM S10-SPLITTA-IMPORTORER                                       
231600     MOVE SPACE TO UTKORT-LONG                                            
231700     .                                                                    
231800     EJECT                                                                
231900 BY-RAD-KRED SECTION.                                                     
232000     SKIP2                                                                
232100     MOVE 'RKE'                    TO RKE-IDPTYP                          
232200     MOVE WSORT-RKE-IDDISTR        TO RKE-IDDISTR                         
232300     MOVE WSORT-RKE-IDKUNDNR       TO RKE-IDKUNDNR                        
232400     MOVE WSORT-RKE-IDDC           TO RKE-IDDC                            
232500     MOVE WSORT-RKE-IDRAPPNR       TO RKE-IDRAPPNR                        
232600     MOVE WSORT-RKE-IDRADNR        TO RKE-IDRADNR                         
232700     MOVE WSORT-RKE-IDARTNR        TO RKE-IDARTNR                         
232800     MOVE WSORT-RKE-REKSIFFR       TO RKE-REKSIFFR                        
232900     MOVE WSORT-RKE-TIRETILL       TO RKE-TIRETILL                        
233000     MOVE WSORT-RKE-IDRAPPNR-002   TO RKE-IDRAPPNR-002                    
233100     PERFORM S10-SPLITTA-IMPORTORER                                       
233200     MOVE SPACE TO UTKORT                                                 
233300     .                                                                    
233400     EJECT                                                                
233500 BZ-RAD-KVITT SECTION.                                                    
233600     SKIP2                                                                
233700     MOVE 'RKF'                TO RKF-IDPTYP                              
233800     MOVE WSORT-IDDC           TO RKF-IDDC                                
233900     MOVE WSORT-IDTABNR        TO RKF-IDTABNR                             
234000     MOVE WSORT-IDARTNR        TO RKF-IDARTNR                             
234100     MOVE WSORT-REKSIFFR       TO RKF-REKSIFFR                            
234200     PERFORM S10-SPLITTA-IMPORTORER                                       
234300     MOVE SPACE TO UTKORT                                                 
234400     .                                                                    
234500     EJECT                                                                
234600 B1-RAD-BYTES SECTION.                                                    
234700     SKIP2                                                                
234800     MOVE 'RKG'                   TO RKG-IDPTYP                           
234900     MOVE WSORT-RKG-IDDISTR       TO RKG-IDDISTR                          
235000     MOVE WSORT-RKG-IDKUNDNR      TO RKG-IDKUNDNR                         
235100     MOVE WSORT-RKG-IDBYTRAP      TO RKG-IDBYTRAP                         
235200     MOVE WSORT-RKG-TIREGDAT-GODK TO RKG-TIREGDAT-GODK                    
235300     MOVE WSORT-RKG-IDARTNR-OBJ   TO RKG-IDARTNR-OBJ                      
235400     MOVE WSORT-RKG-IDTABNR       TO RKG-IDTABNR                          
235500     MOVE WSORT-RKG-KVRETUR-GODK  TO RKG-KVRETUR-GODK                     
235600     MOVE WSORT-RKG-KDBYTSTA-OBJ  TO RKG-KDBYTSTA-OBJ                     
235700     MOVE WSORT-RKG-IDORDNR7      TO RKG-IDORDNR                          
235800     MOVE WSORT-RKG-KDBYTREF      TO RKG-KDBYTREF                         
235900     MOVE WSORT-RKG-IDKUNDRF-GRP  TO RKG-IDKUNDRF                         
236000     MOVE WSORT-RKG-IDBYTRAD      TO RKG-IDBYTRAD                         
236100     MOVE WSORT-RKG-IDDC          TO RKG-IDDC                             
236200     MOVE WSORT-RKG-PRAVCOST-CORE TO RKG-PRAVCOST-CORE                    
236300     PERFORM S10-SPLITTA-IMPORTORER                                       
236400     MOVE SPACE TO UTKORT                                                 
236500     .                                                                    
236600     EJECT                                                                
236700 B2-RAD-KRED-PMN SECTION.                                                 
236800     SKIP2                                                                
236900     MOVE 'RKJ'                    TO RKJ-IDPTYP                          
237000     MOVE WSORT-RKJ-IDDISTR        TO RKJ-IDDISTR                         
237100     MOVE WSORT-RKJ-IDKUNDNR       TO RKJ-IDKUNDNR                        
237200     MOVE WSORT-RKJ-IDRAPPNR       TO RKJ-IDRAPPNR                        
237300     MOVE WSORT-RKJ-IDRADNR        TO RKJ-IDRADNR                         
237400     MOVE WSORT-RKJ-KDANMORS       TO RKJ-KDANMORS                        
237500     MOVE WSORT-RKJ-IDARTNR        TO RKJ-IDARTNR                         
237600     MOVE WSORT-RKJ-KVLEVANM       TO RKJ-KVLEVANM                        
237700     MOVE WSORT-RKJ-DARTPMN        TO RKJ-DARTPMN                         
237800     MOVE WSORT-RKJ-KVDAGAR-RTATG  TO RKJ-KVDAGAR-RTATG                   
237900     PERFORM S10-SPLITTA-IMPORTORER                                       
238000     MOVE SPACE TO UTKORT                                                 
238100     .                                                                    
238200     EJECT                                                                
238300 B3-RAD-POINT SECTION.                                                    
238400     SKIP2                                                                
238500     MOVE 'RKO'                   TO RKO-IDPTYP                           
238600     MOVE WSORT-RKO-IDARTNR       TO RKO-IDARTNR                          
238700     MOVE WSORT-RKO-KVPOINT       TO RKO-KVPOINT                          
238800     MOVE WSORT-RKO-KDEXCHA       TO RKO-KDEXCHA                          
238900     PERFORM S10-SPLITTA-IMPORTORER                                       
239000     MOVE SPACE TO UTKORT                                                 
239100     .                                                                    
239200     EJECT                                                                
239300 S01-LAS-SORTERAD-INFIL SECTION.                                          
239400     SKIP3                                                                
239500     RETURN SORTFIL   INTO WSORT-AREA                                     
239600                      AT END MOVE JA TO SORTFIL-EOF                       
239700     END-RETURN                                                           
239800                                                                          
239900     IF SORTFIL-EOF = NEJ                                                 
240000                                                                          
240100       MOVE 'INFIL'             TO POSTSUM-FDNAMN                         
240200       MOVE 'W46120D1'          TO POSTSUM-DDNAMN2                        
240300       MOVE WSORT-EMB-IDPTYP    TO POSTSUM-TRANSTYP                       
240400       CALL POSTSUM   USING POSTSUM-PARM                                  
240500                                                                          
240600     END-IF                                                               
240700     .                                                                    
240800     EJECT                                                                
240900******************************************************************        
241000*                                                                         
241100*    I S10-SPLITTA-IMPORTORER SECTION VÄXLAS DE OLIKA                     
241200*    IMPORTÖRS-POSTERNA IN PÅ RÄTT FIL.                                   
241300*       BYGG PÅ IF-SATSEN MED NYA DISTRIKT OCH LÄGG                       
241400*    TILL YTTERLIGA S10-SKRIV-W461XX SECTIONER.                           
241500*                                                                         
241600******************************************************************        
241700 S10-SPLITTA-IMPORTORER SECTION.                                          
241800     SKIP2                                                                
241900     MOVE BIP-SOR0-IDDISTR TO TEST-IDDISTR DIS1-IDDISTR                   
242000                                                                          
242100     CALL W460DIS1 USING DIS1-W460DIS1                                    
242200                                                                          
242300     IF (DIS1-IDLANDX2 = ISO-ITALIEN)                                     
242400       OR BIP-SOR0-IDDISTR = ZERO                                         
242500       PERFORM S10-SKRIV-W46127                                           
242600     END-IF                                                               
242700     IF (DIS1-IDLANDX2 = ISO-FINLAND)                                     
242800       OR DIS100-FINLLEVANM                                               
242900       OR BIP-SOR0-IDDISTR = ZERO                                         
243000       PERFORM S10-SKRIV-W46129                                           
243100     END-IF                                                               
243200     IF (DIS1-IDLANDX2 = ISO-BELGIEN)                                     
243300       OR BIP-SOR0-IDDISTR = ZERO                                         
243400       PERFORM S10-SKRIV-W46130                                           
243500     END-IF                                                               
243600     IF (DIS1-IDLANDX2 = ISO-ENGLAND)                                     
243700       OR BIP-SOR0-IDDISTR = ZERO                                         
243800       PERFORM S10-SKRIV-W4612B                                           
243900     END-IF                                                               
244000     IF (DIS1-IDLANDX2 = ISO-IRLAND)                                      
244100       OR BIP-SOR0-IDDISTR = ZERO                                         
244200       PERFORM S10-SKRIV-W4612K                                           
244300     END-IF                                                               
244400     IF ((DIS1-IDLANDX2 = ISO-USA)                                        
244500        AND (DIS102-USA))                                                 
244600       OR BIP-SOR0-IDDISTR = ZERO                                         
244700       PERFORM S10-SKRIV-W46138                                           
244800     END-IF                                                               
244900     IF (DIS1-IDLANDX2 = ISO-TYSKLAND)                                    
245000       OR BIP-SOR0-IDDISTR = ZERO                                         
245100       PERFORM S10-SKRIV-W46171                                           
245200     END-IF                                                               
245300     IF (DIS1-IDLANDX2 = ISO-HOLLAND)                                     
245400       OR BIP-SOR0-IDDISTR = ZERO                                         
245500       PERFORM S10-SKRIV-W46161                                           
245600     END-IF                                                               
245700     IF (DIS1-IDLANDX2 = ISO-SPANIEN)                                     
245800       OR DIS100-SPANLEVANM-PV                                            
245900       OR DIS130-NOAC-SPANIEN                                             
246000       OR BIP-SOR0-IDDISTR = ZERO                                         
246100       PERFORM S10-SKRIV-W46179                                           
246200     END-IF                                                               
246300     IF (DIS1-IDLANDX2 = ISO-OSTERRIKE)                                   
246400       OR BIP-SOR0-IDDISTR = ZERO                                         
246500       PERFORM S10-SKRIV-W46168                                           
246600     END-IF                                                               
246700     IF (DIS1-IDLANDX2 = ISO-SAUDI)                                       
246800       OR BIP-SOR0-IDDISTR = ZERO                                         
246900       PERFORM S10-SKRIV-W46178                                           
247000     END-IF                                                               
247100     IF (DIS1-IDLANDX2 = ISO-PERU)                                        
247200       OR BIP-SOR0-IDDISTR = ZERO                                         
247300       PERFORM S10-SKRIV-W46176                                           
247400     END-IF                                                               
247500     IF (DIS1-IDLANDX2 = ISO-FRANKRIKE)                                   
247600       OR DIS100-FRANLEVANM-PV                                            
247700       OR BIP-SOR0-IDDISTR = ZERO                                         
247800       PERFORM S10-SKRIV-W46173                                           
247900     END-IF                                                               
248000     IF (DIS1-IDLANDX2 = ISO-SVERIGE)                                     
248100       OR BIP-SOR0-IDDISTR = ZERO                                         
248200       PERFORM S10-SKRIV-W46170                                           
248300     END-IF                                                               
248400     IF (DIS1-IDLANDX2 = ISO-DANMARK)                                     
248500       OR DIS100-DANMLEVANM-PV                                            
248600       OR BIP-SOR0-IDDISTR = ZERO                                         
248700       PERFORM S10-SKRIV-W46140                                           
248800     END-IF                                                               
248900     IF (DIS1-IDLANDX2 = ISO-NORGE)                                       
249000       OR DIS100-NORGLEVANM-PV                                            
249100       OR BIP-SOR0-IDDISTR = ZERO                                         
249200       PERFORM S10-SKRIV-W46139                                           
249300     END-IF                                                               
249400     IF DIS130-NOAC-SCHWEIZ-PV                                            
249500       OR BIP-SOR0-IDDISTR = ZERO                                         
249600       PERFORM S10-SKRIV-W46174                                           
249700     END-IF                                                               
249800     IF (DIS1-IDLANDX2 = ISO-BRASILIEN AND DIS121-BRASIL)                 
249900       OR BIP-SOR0-IDDISTR = ZERO                                         
250000       PERFORM S10-SKRIV-W46175                                           
250100     END-IF                                                               
250200     IF (DIS1-IDLANDX2 = ISO-BRASILIEN AND DIS121-BRASIL-NEW)             
250300       OR BIP-SOR0-IDDISTR = ZERO                                         
250400       PERFORM S10-SKRIV-W4612L                                           
250500     END-IF                                                               
250600     IF (DIS1-IDLANDX2 = ISO-MEXICO)                                      
250700       OR DIS130-NOAC-MEXICO                                              
250800       OR BIP-SOR0-IDDISTR = ZERO                                         
250900       PERFORM S10-SKRIV-W4612M                                           
251000     END-IF                                                               
251100     IF (DIS1-IDLANDX2 = ISO-TURKIET)                                     
251200       OR DIS130-NOAC-TURKIET                                             
251300       OR BIP-SOR0-IDDISTR = ZERO                                         
251400       PERFORM S10-SKRIV-W4612N                                           
251500     END-IF                                                               
251600     IF DIS130-NOAC-AUSTRALIEN                                            
251700       OR BIP-SOR0-IDDISTR = ZERO                                         
251800       PERFORM S10-SKRIV-W46131                                           
251900     END-IF                                                               
252000     IF (DIS1-IDLANDX2 = ISO-SCHWEIZ)                                     
252100       OR BIP-SOR0-IDDISTR = ZERO                                         
252200       PERFORM S10-SKRIV-W46172                                           
252300     END-IF                                                               
252400     IF (DIS1-IDLANDX2 = ISO-AUSTRALIEN)                                  
252500       OR BIP-SOR0-IDDISTR = ZERO                                         
252600       PERFORM S10-SKRIV-W46180                                           
252700     END-IF                                                               
252800     IF (DIS1-IDLANDX2 = ISO-TAIWAN)                                      
252900       OR DIS130-NOAC-TAIWAN                                              
253000       OR BIP-SOR0-IDDISTR = ZERO                                         
253100       PERFORM S10-SKRIV-W46134                                           
253200     END-IF                                                               
253300     IF (DIS1-IDLANDX2 = ISO-TAIWAN2)                                     
253400       OR DIS130-NOAC-TAIWAN2                                             
253500       OR BIP-SOR0-IDDISTR = ZERO                                         
253600       PERFORM S10-SKRIV-W4612A                                           
253700     END-IF                                                               
253800     IF DIS1-IDLANDX2 = ISO-JAPAN                                         
253900       OR BIP-SOR0-IDDISTR = ZERO                                         
254000       PERFORM S10-SKRIV-W4612H                                           
254100     END-IF                                                               
254200     IF (DIS1-IDLANDX2 = ISO-THAILAND)                                    
254300       OR DIS130-NOAC-THAILAND                                            
254400       OR BIP-SOR0-IDDISTR = ZERO                                         
254500       PERFORM S10-SKRIV-W4612J                                           
254600     END-IF                                                               
254700     IF (DIS1-IDLANDX2 = ISO-MALAYSIA)                                    
254800       OR DIS130-NOAC-MALAYSIA                                            
254900       OR BIP-SOR0-IDDISTR = ZERO                                         
255000       PERFORM S10-SKRIV-W4612I                                           
255100     END-IF                                                               
255200     IF DIS100-JAPANLEVANM                                                
255300       OR BIP-SOR0-IDDISTR = ZERO                                         
255400       PERFORM S10-SKRIV-W46128                                           
255500     END-IF                                                               
255600     IF (DIS1-IDLANDX2 = ISO-POLEN)                                       
255700       OR DIS130-NOAC-POLEN                                               
255800       OR BIP-SOR0-IDDISTR = ZERO                                         
255900       PERFORM S10-SKRIV-W4612C                                           
256000     END-IF                                                               
256100     IF DIS130-NOAC-FINLAND                                               
256200       OR BIP-SOR0-IDDISTR = ZERO                                         
256300       PERFORM S10-SKRIV-W4612Z                                           
256400     END-IF                                                               
256500     IF ((DIS1-IDLANDX2 = ISO-USA)                                        
256600       AND  (DIS102-USA-NEW))                                             
256700       OR BIP-SOR0-IDDISTR = ZERO                                         
256800       PERFORM S10-SKRIV-W4612E                                           
256900     END-IF                                                               
257000     IF (DIS1-IDLANDX2 = ISO-CANADA)                                      
257100       OR BIP-SOR0-IDDISTR = ZERO                                         
257200       PERFORM S10-SKRIV-W4612D                                           
257300     END-IF                                                               
257400     IF (DIS1-IDLANDX2 = ISO-KOREA)                                       
257500       OR BIP-SOR0-IDDISTR = ZERO                                         
257600       PERFORM S10-SKRIV-W4612F                                           
257700     END-IF                                                               
257800     IF (DIS1-IDLANDX2 = ISO-PORTUGAL)                                    
257900       OR BIP-SOR0-IDDISTR = ZERO                                         
258000       PERFORM S10-SKRIV-W4612G                                           
258100     END-IF                                                               
258200     IF (DIS1-IDLANDX2 = ISO-RYSSLAND)                                    
258300       OR BIP-SOR0-IDDISTR = ZERO                                         
258400       PERFORM S10-SKRIV-W4612P                                           
258500     END-IF                                                               
258600     IF (DIS1-IDLANDX2 = ISO-KINA)                                        
258700       OR BIP-SOR0-IDDISTR = ZERO                                         
258800       PERFORM S10-SKRIV-W461CN                                           
258900     END-IF                                                               
259000     IF (DIS1-IDLANDX2 = ISO-KINA-C1)                                     
259100       OR BIP-SOR0-IDDISTR = ZERO                                         
259200       PERFORM S10-SKRIV-W461CN1                                          
259300     END-IF                                                               
259400     IF (DIS1-IDLANDX2 = ISO-SYDAFRIKA)                                   
259500       OR DIS130-NOAC-SYDAFRIKA                                           
259510       OR BIP-SOR0-IDDISTR = ZERO                                         
259600       PERFORM S10-SKRIV-W461ZA                                           
259700     END-IF                                                               
259800     IF (DIS1-IDLANDX2 = ISO-PORTUGAL2)                                   
259900       OR DIS130-NOAC-PORTUGAL                                            
260000       OR BIP-SOR0-IDDISTR = ZERO                                         
260100       PERFORM S10-SKRIV-W461PT3                                          
260200     END-IF                                                               
260301     IF (DIS1-IDLANDX2 = ISO-INDIEN)                                      
260401       OR BIP-SOR0-IDDISTR = ZERO                                         
260501       PERFORM S10-SKRIV-W46126                                           
260601     END-IF                                                               
260701     IF (DIS1-IDLANDX2 = ISO-TJECKIEN)                                    
260801       OR BIP-SOR0-IDDISTR = ZERO                                         
260901       PERFORM S10-SKRIV-W46122                                           
261001     END-IF                                                               
261101     IF (DIS1-IDLANDX2 = ISO-UNGERN)                                      
261201       OR BIP-SOR0-IDDISTR = ZERO                                         
261301       PERFORM S10-SKRIV-W46123                                           
261401     END-IF                                                               
261500     .                                                                    
261600     EJECT                                                                
261700 S10-SKRIV-W46127 SECTION.                                                
261800     SKIP2                                                                
261900     EVALUATE RID-IDPTYP                                                  
262000       WHEN 'RID'                                                         
262100         WRITE RID-POST-IT FROM UTKORT-LONG                               
262200         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
262300       WHEN 'RIE'                                                         
262400         WRITE RIE-POST-IT FROM UTKORT-LONG                               
262500         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
262600       WHEN 'RIH'                                                         
262700         WRITE RIH-POST-IT FROM UTKORT-LONG                               
262800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
262900       WHEN 'RIO'                                                         
263000         WRITE SOFT-RIO-POST-IT FROM UTKORT-LONG                          
263100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
263200       WHEN 'RKB'                                                         
263300         WRITE RKB-POST-IT FROM UTKORT-LONG                               
263400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
263500       WHEN 'RKC'                                                         
263600         WRITE RKC-POST-IT FROM UTKORT-LONG                               
263700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
263800       WHEN 'RKD'                                                         
263900         WRITE RKD-POST-IT FROM UTKORT-LONG                               
264000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
264100       WHEN OTHER                                                         
264200         WRITE IMP-IT FROM UTKORT                                         
264300         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
264400     END-EVALUATE                                                         
264500     MOVE 'W46127'               TO POSTSUM-FDNAMN                        
264600     MOVE 'W46120D3'             TO POSTSUM-DDNAMN2                       
264700     CALL POSTSUM      USING POSTSUM-PARM                                 
264800     .                                                                    
264900     EJECT                                                                
265000 S10-SKRIV-W46129 SECTION.                                                
265100     SKIP2                                                                
265200     EVALUATE RID-IDPTYP                                                  
265300       WHEN 'RID'                                                         
265400         WRITE RID-POST-FI FROM UTKORT-LONG                               
265500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
265600       WHEN 'RIE'                                                         
265700         WRITE RIE-POST-FI FROM UTKORT-LONG                               
265800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
265900       WHEN 'RIH'                                                         
266000         WRITE RIH-POST-FI FROM UTKORT-LONG                               
266100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
266200       WHEN 'RIO'                                                         
266300         WRITE SOFT-RIO-POST-FI FROM UTKORT-LONG                          
266400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
266500       WHEN 'RKB'                                                         
266600         WRITE RKB-POST-FI FROM UTKORT-LONG                               
266700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
266800       WHEN 'RKC'                                                         
266900         WRITE RKC-POST-FI FROM UTKORT-LONG                               
267000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
267100       WHEN 'RKD'                                                         
267200         WRITE RKD-POST-FI FROM UTKORT-LONG                               
267300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
267400       WHEN OTHER                                                         
267500         WRITE IMP-FI FROM UTKORT                                         
267600         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
267700     END-EVALUATE                                                         
267800     MOVE 'W46129'               TO POSTSUM-FDNAMN                        
267900     MOVE 'W46120D4'             TO POSTSUM-DDNAMN2                       
268000     CALL POSTSUM      USING POSTSUM-PARM                                 
268100     .                                                                    
268200     EJECT                                                                
268300 S10-SKRIV-W46130 SECTION.                                                
268400     SKIP2                                                                
268500     EVALUATE RID-IDPTYP                                                  
268600       WHEN 'RID'                                                         
268700         WRITE RID-POST-BE FROM UTKORT-LONG                               
268800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
268900       WHEN 'RIE'                                                         
269000         WRITE RIE-POST-BE FROM UTKORT-LONG                               
269100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
269200       WHEN 'RIH'                                                         
269300         WRITE RIH-POST-BE FROM UTKORT-LONG                               
269400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
269500       WHEN 'RIO'                                                         
269600         WRITE SOFT-RIO-POST-BE FROM UTKORT-LONG                          
269700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
269800       WHEN 'RKB'                                                         
269900         WRITE RKB-POST-BE FROM UTKORT-LONG                               
270000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
270100       WHEN 'RKC'                                                         
270200         WRITE RKC-POST-BE FROM UTKORT-LONG                               
270300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
270400       WHEN 'RKD'                                                         
270500         WRITE RKD-POST-BE FROM UTKORT-LONG                               
270600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
270700       WHEN OTHER                                                         
270800         WRITE IMP-BE FROM UTKORT                                         
270900         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
271000     END-EVALUATE                                                         
271100     MOVE 'W46130'               TO POSTSUM-FDNAMN                        
271200     MOVE 'W46120D5'             TO POSTSUM-DDNAMN2                       
271300     CALL POSTSUM      USING POSTSUM-PARM                                 
271400     .                                                                    
271500     EJECT                                                                
271600 S10-SKRIV-W46138 SECTION.                                                
271700     SKIP2                                                                
271800     EVALUATE RID-IDPTYP                                                  
271900       WHEN 'RID'                                                         
272000         WRITE RID-POST-US FROM UTKORT-LONG                               
272100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
272200       WHEN 'RIE'                                                         
272300         WRITE RIE-POST-US FROM UTKORT-LONG                               
272400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
272500       WHEN 'RIH'                                                         
272600         WRITE RIH-POST-US FROM UTKORT-LONG                               
272700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
272800       WHEN 'RIO'                                                         
272900         WRITE SOFT-RIO-POST-US FROM UTKORT-LONG                          
273000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
273100       WHEN 'RKB'                                                         
273200         WRITE RKB-POST-US FROM UTKORT-LONG                               
273300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
273400       WHEN 'RKC'                                                         
273500         WRITE RKC-POST-US FROM UTKORT-LONG                               
273600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
273700       WHEN 'RKD'                                                         
273800         WRITE RKD-POST-US FROM UTKORT-LONG                               
273900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
274000       WHEN OTHER                                                         
274100         WRITE IMP-US FROM UTKORT                                         
274200         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
274300     END-EVALUATE                                                         
274400     MOVE 'W46138'               TO POSTSUM-FDNAMN                        
274500     MOVE 'W46120D8'             TO POSTSUM-DDNAMN2                       
274600     CALL POSTSUM      USING POSTSUM-PARM                                 
274700     .                                                                    
274800     EJECT                                                                
274900 S10-SKRIV-W46171 SECTION.                                                
275000     SKIP2                                                                
275100     EVALUATE RID-IDPTYP                                                  
275200       WHEN 'RID'                                                         
275300         WRITE RID-POST-DE FROM UTKORT-LONG                               
275400         MOVE RID-IDPTYP       TO POSTSUM-TRANSTYP                        
275500       WHEN 'RIE'                                                         
275600         WRITE RIE-POST-DE FROM UTKORT-LONG                               
275700         MOVE RID-IDPTYP       TO POSTSUM-TRANSTYP                        
275800       WHEN 'RIH'                                                         
275900         WRITE RIH-POST-DE FROM UTKORT-LONG                               
276000         MOVE RID-IDPTYP       TO POSTSUM-TRANSTYP                        
276100       WHEN 'RIO'                                                         
276200         WRITE SOFT-RIO-POST-DE FROM UTKORT-LONG                          
276300         MOVE RID-IDPTYP       TO POSTSUM-TRANSTYP                        
276400       WHEN 'RKB'                                                         
276500         WRITE RKB-POST-DE FROM UTKORT-LONG                               
276600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
276700       WHEN 'RKC'                                                         
276800         WRITE RKC-POST-DE FROM UTKORT-LONG                               
276900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
277000       WHEN 'RKD'                                                         
277100         WRITE RKD-POST-DE FROM UTKORT-LONG                               
277200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
277300       WHEN OTHER                                                         
277400         WRITE IMP-DE FROM UTKORT                                         
277500         MOVE RIA-IDPTYP       TO POSTSUM-TRANSTYP                        
277600     END-EVALUATE                                                         
277700     MOVE 'W46171'               TO POSTSUM-FDNAMN                        
277800     MOVE 'W46120D9'             TO POSTSUM-DDNAMN2                       
277900     CALL POSTSUM      USING POSTSUM-PARM                                 
278000     .                                                                    
278100     EJECT                                                                
278200 S10-SKRIV-W46161 SECTION.                                                
278300     SKIP2                                                                
278400     EVALUATE RID-IDPTYP                                                  
278500       WHEN 'RID'                                                         
278600         WRITE RID-POST-NL FROM UTKORT-LONG                               
278700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
278800       WHEN 'RIE'                                                         
278900         WRITE RIE-POST-NL FROM UTKORT-LONG                               
279000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
279100       WHEN 'RIH'                                                         
279200         WRITE RIH-POST-NL FROM UTKORT-LONG                               
279300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
279400       WHEN 'RIO'                                                         
279500         WRITE SOFT-RIO-POST-NL FROM UTKORT-LONG                          
279600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
279700       WHEN 'RKB'                                                         
279800         WRITE RKB-POST-NL FROM UTKORT-LONG                               
279900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
280000       WHEN 'RKC'                                                         
280100         WRITE RKC-POST-NL FROM UTKORT-LONG                               
280200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
280300       WHEN 'RKD'                                                         
280400         WRITE RKD-POST-NL FROM UTKORT-LONG                               
280500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
280600       WHEN OTHER                                                         
280700         WRITE IMP-NL FROM UTKORT                                         
280800         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
280900     END-EVALUATE                                                         
281000     MOVE 'W46161'               TO POSTSUM-FDNAMN                        
281100     MOVE 'W46120DA'             TO POSTSUM-DDNAMN2                       
281200     CALL POSTSUM      USING POSTSUM-PARM                                 
281300     .                                                                    
281400     EJECT                                                                
281500 S10-SKRIV-W46179 SECTION.                                                
281600     SKIP2                                                                
281700     EVALUATE RID-IDPTYP                                                  
281800       WHEN 'RID'                                                         
281900         WRITE RID-POST-ES FROM UTKORT-LONG                               
282000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
282100       WHEN 'RIE'                                                         
282200         WRITE RIE-POST-ES FROM UTKORT-LONG                               
282300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
282400       WHEN 'RIH'                                                         
282500         WRITE RIH-POST-ES FROM UTKORT-LONG                               
282600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
282700       WHEN 'RIO'                                                         
282800         WRITE SOFT-RIO-POST-ES FROM UTKORT-LONG                          
282900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
283000       WHEN 'RKB'                                                         
283100         WRITE RKB-POST-ES FROM UTKORT-LONG                               
283200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
283300       WHEN 'RKC'                                                         
283400         WRITE RKC-POST-ES FROM UTKORT-LONG                               
283500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
283600       WHEN 'RKD'                                                         
283700         WRITE RKD-POST-ES FROM UTKORT-LONG                               
283800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
283900       WHEN OTHER                                                         
284000         WRITE IMP-ES FROM UTKORT                                         
284100         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
284200     END-EVALUATE                                                         
284300     MOVE 'W46179'               TO POSTSUM-FDNAMN                        
284400     MOVE 'W46120DB'             TO POSTSUM-DDNAMN2                       
284500     CALL POSTSUM      USING POSTSUM-PARM                                 
284600     .                                                                    
284700     EJECT                                                                
284800 S10-SKRIV-W46168 SECTION.                                                
284900     SKIP2                                                                
285000     EVALUATE RID-IDPTYP                                                  
285100       WHEN 'RID'                                                         
285200         WRITE RID-POST-AT FROM UTKORT-LONG                               
285300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
285400       WHEN 'RIE'                                                         
285500         WRITE RIE-POST-AT FROM UTKORT-LONG                               
285600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
285700       WHEN 'RIH'                                                         
285800         WRITE RIH-POST-AT FROM UTKORT-LONG                               
285900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
286000       WHEN 'RIO'                                                         
286100         WRITE SOFT-RIO-POST-AT FROM UTKORT-LONG                          
286200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
286300       WHEN 'RKB'                                                         
286400         WRITE RKB-POST-AT FROM UTKORT-LONG                               
286500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
286600       WHEN 'RKC'                                                         
286700         WRITE RKC-POST-AT FROM UTKORT-LONG                               
286800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
286900       WHEN 'RKD'                                                         
287000         WRITE RKD-POST-AT FROM UTKORT-LONG                               
287100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
287200       WHEN OTHER                                                         
287300         WRITE IMP-AT FROM UTKORT                                         
287400         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
287500     END-EVALUATE                                                         
287600     MOVE 'W46168'               TO POSTSUM-FDNAMN                        
287700     MOVE 'W46120DC'             TO POSTSUM-DDNAMN2                       
287800     CALL POSTSUM      USING POSTSUM-PARM                                 
287900     .                                                                    
288000     EJECT                                                                
288100 S10-SKRIV-W46178 SECTION.                                                
288200     SKIP2                                                                
288300     EVALUATE RID-IDPTYP                                                  
288400       WHEN 'RID'                                                         
288500         WRITE RID-POST-SA FROM UTKORT-LONG                               
288600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
288700       WHEN 'RIE'                                                         
288800         WRITE RIE-POST-SA FROM UTKORT-LONG                               
288900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
289000       WHEN 'RIH'                                                         
289100         WRITE RIH-POST-SA FROM UTKORT-LONG                               
289200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
289300       WHEN 'RIO'                                                         
289400         WRITE RIO-POST-SA FROM UTKORT-LONG                               
289500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
289600       WHEN 'RKB'                                                         
289700         WRITE RKB-POST-SA FROM UTKORT-LONG                               
289800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
289900       WHEN 'RKC'                                                         
290000         WRITE RKC-POST-SA FROM UTKORT-LONG                               
290100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
290200       WHEN 'RKD'                                                         
290300         WRITE RKD-POST-SA FROM UTKORT-LONG                               
290400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
290500       WHEN OTHER                                                         
290600         WRITE IMP-SA FROM UTKORT                                         
290700         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
290800     END-EVALUATE                                                         
290900     MOVE 'W46178'               TO POSTSUM-FDNAMN                        
291000     MOVE 'W46120DD'             TO POSTSUM-DDNAMN2                       
291100     CALL POSTSUM      USING POSTSUM-PARM                                 
291200     .                                                                    
291300     EJECT                                                                
291400 S10-SKRIV-W46176 SECTION.                                                
291500     SKIP2                                                                
291600     EVALUATE RID-IDPTYP                                                  
291700       WHEN 'RID'                                                         
291800         WRITE RID-POST-PE FROM UTKORT-LONG                               
291900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
292000       WHEN 'RIE'                                                         
292100         WRITE RIE-POST-PE FROM UTKORT-LONG                               
292200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
292300       WHEN 'RIH'                                                         
292400         WRITE RIH-POST-PE FROM UTKORT-LONG                               
292500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
292600       WHEN 'RIO'                                                         
292700         WRITE RIO-POST-PE FROM UTKORT-LONG                               
292800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
292900       WHEN 'RKB'                                                         
293000         WRITE RKB-POST-PE FROM UTKORT-LONG                               
293100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
293200       WHEN 'RKC'                                                         
293300         WRITE RKC-POST-PE FROM UTKORT-LONG                               
293400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
293500       WHEN 'RKD'                                                         
293600         WRITE RKD-POST-PE FROM UTKORT-LONG                               
293700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
293800       WHEN OTHER                                                         
293900         WRITE IMP-PE FROM UTKORT                                         
294000         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
294100     END-EVALUATE                                                         
294200     MOVE 'W46176'               TO POSTSUM-FDNAMN                        
294300     MOVE 'W46120DE'             TO POSTSUM-DDNAMN2                       
294400     CALL POSTSUM      USING POSTSUM-PARM                                 
294500     .                                                                    
294600     EJECT                                                                
294700 S10-SKRIV-W46173 SECTION.                                                
294800     SKIP2                                                                
294900     EVALUATE RID-IDPTYP                                                  
295000       WHEN 'RID'                                                         
295100         WRITE RID-POST-FR FROM UTKORT-LONG                               
295200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
295300       WHEN 'RIE'                                                         
295400         WRITE RIE-POST-FR FROM UTKORT-LONG                               
295500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
295600       WHEN 'RIH'                                                         
295700         WRITE RIH-POST-FR FROM UTKORT-LONG                               
295800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
295900       WHEN 'RIO'                                                         
296000         WRITE SOFT-RIO-POST-FR FROM UTKORT-LONG                          
296100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
296200       WHEN 'RKB'                                                         
296300         WRITE RKB-POST-FR FROM UTKORT-LONG                               
296400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
296500       WHEN 'RKC'                                                         
296600         WRITE RKC-POST-FR FROM UTKORT-LONG                               
296700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
296800       WHEN 'RKD'                                                         
296900         WRITE RKD-POST-FR FROM UTKORT-LONG                               
297000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
297100       WHEN OTHER                                                         
297200         WRITE IMP-FR FROM UTKORT                                         
297300         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
297400     END-EVALUATE                                                         
297500     MOVE 'W46173'               TO POSTSUM-FDNAMN                        
297600     MOVE 'W46120DF'             TO POSTSUM-DDNAMN2                       
297700     CALL POSTSUM      USING POSTSUM-PARM                                 
297800     .                                                                    
297900     EJECT                                                                
298000 S10-SKRIV-W46170 SECTION.                                                
298100     SKIP2                                                                
298200     EVALUATE RID-IDPTYP                                                  
298300       WHEN 'RID'                                                         
298400         WRITE RID-POST-SE FROM UTKORT-LONG                               
298500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
298600       WHEN 'RIE'                                                         
298700         WRITE RIE-POST-SE FROM UTKORT-LONG                               
298800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
298900       WHEN 'RIH'                                                         
299000         WRITE RIH-POST-SE FROM UTKORT-LONG                               
299100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
299200       WHEN 'RIO'                                                         
299300         WRITE SOFT-RIO-POST-SE FROM UTKORT-LONG                          
299400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
299500       WHEN 'RKB'                                                         
299600         WRITE RKB-POST-SE FROM UTKORT-LONG                               
299700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
299800       WHEN 'RKC'                                                         
299900         WRITE RKC-POST-SE FROM UTKORT-LONG                               
300000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
300100       WHEN 'RKD'                                                         
300200         WRITE RKD-POST-SE FROM UTKORT-LONG                               
300300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
300400       WHEN OTHER                                                         
300500         WRITE IMP-SE FROM UTKORT                                         
300600         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
300700     END-EVALUATE                                                         
300800     MOVE 'W46170'               TO POSTSUM-FDNAMN                        
300900     MOVE 'W46120DG'             TO POSTSUM-DDNAMN2                       
301000     CALL POSTSUM      USING POSTSUM-PARM                                 
301100     .                                                                    
301200     EJECT                                                                
301300 S10-SKRIV-W46140 SECTION.                                                
301400     SKIP2                                                                
301500     EVALUATE RID-IDPTYP                                                  
301600       WHEN 'RID'                                                         
301700         WRITE RID-POST-DK FROM UTKORT-LONG                               
301800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
301900       WHEN 'RIE'                                                         
302000         WRITE RIE-POST-DK FROM UTKORT-LONG                               
302100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
302200       WHEN 'RIH'                                                         
302300         WRITE RIH-POST-DK FROM UTKORT-LONG                               
302400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
302500       WHEN 'RIO'                                                         
302600         WRITE SOFT-RIO-POST-DK FROM UTKORT-LONG                          
302700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
302800       WHEN 'RKB'                                                         
302900         WRITE RKB-POST-DK FROM UTKORT-LONG                               
303000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
303100       WHEN 'RKC'                                                         
303200         WRITE RKC-POST-DK FROM UTKORT-LONG                               
303300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
303400       WHEN 'RKD'                                                         
303500         WRITE RKD-POST-DK FROM UTKORT-LONG                               
303600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
303700       WHEN OTHER                                                         
303800         WRITE IMP-DK FROM UTKORT                                         
303900         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
304000     END-EVALUATE                                                         
304100     MOVE 'W46140'               TO POSTSUM-FDNAMN                        
304200     MOVE 'W46120DH'             TO POSTSUM-DDNAMN2                       
304300     CALL POSTSUM      USING POSTSUM-PARM                                 
304400     .                                                                    
304500     EJECT                                                                
304600 S10-SKRIV-W46139 SECTION.                                                
304700     SKIP2                                                                
304800     EVALUATE RID-IDPTYP                                                  
304900       WHEN 'RID'                                                         
305000         WRITE RID-POST-NO FROM UTKORT-LONG                               
305100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
305200       WHEN 'RIE'                                                         
305300         WRITE RIE-POST-NO FROM UTKORT-LONG                               
305400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
305500       WHEN 'RIH'                                                         
305600         WRITE RIH-POST-NO FROM UTKORT-LONG                               
305700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
305800       WHEN 'RIO'                                                         
305900         WRITE SOFT-RIO-POST-NO FROM UTKORT-LONG                          
306000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
306100       WHEN 'RKB'                                                         
306200         WRITE RKB-POST-NO FROM UTKORT-LONG                               
306300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
306400       WHEN 'RKC'                                                         
306500         WRITE RKC-POST-NO FROM UTKORT-LONG                               
306600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
306700       WHEN 'RKD'                                                         
306800         WRITE RKD-POST-NO FROM UTKORT-LONG                               
306900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
307000       WHEN OTHER                                                         
307100         WRITE IMP-NO FROM UTKORT                                         
307200         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
307300     END-EVALUATE                                                         
307400     MOVE 'W46139'               TO POSTSUM-FDNAMN                        
307500     MOVE 'W46120DI'             TO POSTSUM-DDNAMN2                       
307600     CALL POSTSUM      USING POSTSUM-PARM                                 
307700     .                                                                    
307800     EJECT                                                                
307900 S10-SKRIV-W46174 SECTION.                                                
308000     SKIP2                                                                
308100     EVALUATE RID-IDPTYP                                                  
308200       WHEN 'RID'                                                         
308300         WRITE RID-POST-CH2070 FROM UTKORT-LONG                           
308400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
308500       WHEN 'RIE'                                                         
308600         WRITE RIE-POST-CH2070 FROM UTKORT-LONG                           
308700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
308800       WHEN 'RIH'                                                         
308900         WRITE RIH-POST-CH2070 FROM UTKORT-LONG                           
309000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
309100       WHEN 'RIO'                                                         
309200         WRITE SOFT-RIO-POST-CH2070 FROM UTKORT-LONG                      
309300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
309400       WHEN 'RKB'                                                         
309500         WRITE RKB-POST-CH2070 FROM UTKORT-LONG                           
309600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
309700       WHEN 'RKC'                                                         
309800         WRITE RKC-POST-CH2070 FROM UTKORT-LONG                           
309900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
310000       WHEN 'RKD'                                                         
310100         WRITE RKD-POST-CH2070 FROM UTKORT-LONG                           
310200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
310300       WHEN OTHER                                                         
310400         WRITE IMP-CH2070 FROM UTKORT                                     
310500         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
310600     END-EVALUATE                                                         
310700     MOVE 'W46174'               TO POSTSUM-FDNAMN                        
310800     MOVE 'W46120DJ'             TO POSTSUM-DDNAMN2                       
310900     CALL POSTSUM      USING POSTSUM-PARM                                 
311000     .                                                                    
311100     EJECT                                                                
311200 S10-SKRIV-W46175 SECTION.                                                
311300     SKIP2                                                                
311400     EVALUATE RID-IDPTYP                                                  
311500       WHEN 'RID'                                                         
311600         WRITE RID-POST-BR FROM UTKORT-LONG                               
311700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
311800       WHEN 'RIE'                                                         
311900         WRITE RIE-POST-BR FROM UTKORT-LONG                               
312000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
312100       WHEN 'RIH'                                                         
312200         WRITE RIH-POST-BR FROM UTKORT-LONG                               
312300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
312400       WHEN 'RIO'                                                         
312500         WRITE RIO-POST-BR FROM UTKORT-LONG                               
312600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
312700       WHEN 'RKB'                                                         
312800         WRITE RKB-POST-BR FROM UTKORT-LONG                               
312900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
313000       WHEN 'RKC'                                                         
313100         WRITE RKC-POST-BR FROM UTKORT-LONG                               
313200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
313300       WHEN 'RKD'                                                         
313400         WRITE RKD-POST-BR FROM UTKORT-LONG                               
313500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
313600       WHEN OTHER                                                         
313700         WRITE IMP-BR FROM UTKORT                                         
313800         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
313900     END-EVALUATE                                                         
314000     MOVE 'W46175'               TO POSTSUM-FDNAMN                        
314100     MOVE 'W46120DK'             TO POSTSUM-DDNAMN2                       
314200     CALL POSTSUM      USING POSTSUM-PARM                                 
314300     .                                                                    
314400     EJECT                                                                
314500 S10-SKRIV-W46131 SECTION.                                                
314600     SKIP2                                                                
314700     EVALUATE RID-IDPTYP                                                  
314800       WHEN 'RID'                                                         
314900         WRITE RID-POST-AU7836 FROM UTKORT-LONG                           
315000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
315100       WHEN 'RIE'                                                         
315200         WRITE RIE-POST-AU7836 FROM UTKORT-LONG                           
315300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
315400       WHEN 'RIH'                                                         
315500         WRITE RIH-POST-AU7836 FROM UTKORT-LONG                           
315600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
315700       WHEN 'RIO'                                                         
315800         WRITE SOFT-RIO-POST-AU7836 FROM UTKORT-LONG                      
315900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
316000       WHEN 'RKB'                                                         
316100         WRITE RKB-POST-AU7836 FROM UTKORT-LONG                           
316200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
316300       WHEN 'RKC'                                                         
316400         WRITE RKC-POST-AU7836 FROM UTKORT-LONG                           
316500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
316600       WHEN 'RKD'                                                         
316700         WRITE RKD-POST-AU7836 FROM UTKORT-LONG                           
316800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
316900       WHEN OTHER                                                         
317000         WRITE IMP-AU7836 FROM UTKORT                                     
317100         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
317200     END-EVALUATE                                                         
317300     MOVE 'W46131'               TO POSTSUM-FDNAMN                        
317400     MOVE 'W46120DL'             TO POSTSUM-DDNAMN2                       
317500     CALL POSTSUM      USING POSTSUM-PARM                                 
317600     .                                                                    
317700     EJECT                                                                
317800 S10-SKRIV-W46172 SECTION.                                                
317900     SKIP2                                                                
318000     EVALUATE RID-IDPTYP                                                  
318100       WHEN 'RID'                                                         
318200         WRITE RID-POST-CH2078 FROM UTKORT-LONG                           
318300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
318400       WHEN 'RIE'                                                         
318500         WRITE RIE-POST-CH2078 FROM UTKORT-LONG                           
318600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
318700       WHEN 'RIH'                                                         
318800         WRITE RIH-POST-CH2078 FROM UTKORT-LONG                           
318900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
319000       WHEN 'RIO'                                                         
319100         WRITE SOFT-RIO-POST-CH2078 FROM UTKORT-LONG                      
319200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
319300       WHEN 'RKB'                                                         
319400         WRITE RKB-POST-CH2078 FROM UTKORT-LONG                           
319500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
319600       WHEN 'RKC'                                                         
319700         WRITE RKC-POST-CH2078 FROM UTKORT-LONG                           
319800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
319900       WHEN 'RKD'                                                         
320000         WRITE RKD-POST-CH2078 FROM UTKORT-LONG                           
320100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
320200       WHEN OTHER                                                         
320300         WRITE IMP-CH2078 FROM UTKORT                                     
320400         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
320500     END-EVALUATE                                                         
320600     MOVE 'W46172'               TO POSTSUM-FDNAMN                        
320700     MOVE 'W46120D6'             TO POSTSUM-DDNAMN2                       
320800     CALL POSTSUM      USING POSTSUM-PARM                                 
320900     .                                                                    
321000     EJECT                                                                
321100 S10-SKRIV-W46180 SECTION.                                                
321200     SKIP2                                                                
321300     EVALUATE RID-IDPTYP                                                  
321400       WHEN 'RID'                                                         
321500         WRITE RID-POST-AU7838 FROM UTKORT-LONG                           
321600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
321700       WHEN 'RIE'                                                         
321800         WRITE RIE-POST-AU7838 FROM UTKORT-LONG                           
321900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
322000       WHEN 'RIH'                                                         
322100         WRITE RIH-POST-AU7838 FROM UTKORT-LONG                           
322200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
322300       WHEN 'RIO'                                                         
322400         WRITE SOFT-RIO-POST-AU7838 FROM UTKORT-LONG                      
322500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
322600       WHEN 'RKB'                                                         
322700         WRITE RKB-POST-AU7838 FROM UTKORT-LONG                           
322800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
322900       WHEN 'RKC'                                                         
323000         WRITE RKC-POST-AU7838 FROM UTKORT-LONG                           
323100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
323200       WHEN 'RKD'                                                         
323300         WRITE RKD-POST-AU7838 FROM UTKORT-LONG                           
323400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
323500       WHEN OTHER                                                         
323600         WRITE IMP-AU7838 FROM UTKORT                                     
323700         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
323800     END-EVALUATE                                                         
323900     MOVE 'W46180'               TO POSTSUM-FDNAMN                        
324000     MOVE 'W46120DM'             TO POSTSUM-DDNAMN2                       
324100     CALL POSTSUM      USING POSTSUM-PARM                                 
324200     .                                                                    
324300     EJECT                                                                
324400 S10-SKRIV-W46134 SECTION.                                                
324500     SKIP2                                                                
324600     EVALUATE RID-IDPTYP                                                  
324700       WHEN 'RID'                                                         
324800         WRITE RID-POST-TW FROM UTKORT-LONG                               
324900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
325000       WHEN 'RIE'                                                         
325100         WRITE RIE-POST-TW FROM UTKORT-LONG                               
325200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
325300       WHEN 'RIH'                                                         
325400         WRITE RIH-POST-TW FROM UTKORT-LONG                               
325500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
325600       WHEN 'RIO'                                                         
325700         WRITE SOFT-RIO-POST-TW FROM UTKORT-LONG                          
325800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
325900       WHEN 'RKB'                                                         
326000         WRITE RKB-POST-TW FROM UTKORT-LONG                               
326100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
326200       WHEN 'RKC'                                                         
326300         WRITE RKC-POST-TW FROM UTKORT-LONG                               
326400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
326500       WHEN 'RKD'                                                         
326600         WRITE RKD-POST-TW FROM UTKORT-LONG                               
326700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
326800       WHEN OTHER                                                         
326900         WRITE IMP-TW FROM UTKORT                                         
327000         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
327100     END-EVALUATE                                                         
327200     MOVE 'W46134'               TO POSTSUM-FDNAMN                        
327300     MOVE 'W46120DO'             TO POSTSUM-DDNAMN2                       
327400     CALL POSTSUM      USING POSTSUM-PARM                                 
327500     .                                                                    
327600     EJECT                                                                
327700 S10-SKRIV-W4612A SECTION.                                                
327800     SKIP2                                                                
327900     EVALUATE RID-IDPTYP                                                  
328000       WHEN 'RID'                                                         
328100         WRITE RID-POST-TW2 FROM UTKORT-LONG                              
328200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
328300       WHEN 'RIE'                                                         
328400         WRITE RIE-POST-TW2 FROM UTKORT-LONG                              
328500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
328600       WHEN 'RIH'                                                         
328700         WRITE RIH-POST-TW2 FROM UTKORT-LONG                              
328800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
328900       WHEN 'RIO'                                                         
329000         WRITE SOFT-RIO-POST-TW2 FROM UTKORT-LONG                         
329100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
329200       WHEN 'RKB'                                                         
329300         WRITE RKB-POST-TW2 FROM UTKORT-LONG                              
329400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
329500       WHEN 'RKC'                                                         
329600         WRITE RKC-POST-TW2 FROM UTKORT-LONG                              
329700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
329800       WHEN 'RKD'                                                         
329900         WRITE RKD-POST-TW2 FROM UTKORT-LONG                              
330000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
330100       WHEN OTHER                                                         
330200         WRITE IMP-TW2 FROM UTKORT                                        
330300         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
330400     END-EVALUATE                                                         
330500     MOVE 'W4612A'               TO POSTSUM-FDNAMN                        
330600     MOVE 'W46120DQ'             TO POSTSUM-DDNAMN2                       
330700     CALL POSTSUM      USING POSTSUM-PARM                                 
330800     .                                                                    
330900     EJECT                                                                
331000 S10-SKRIV-W4612H SECTION.                                                
331100     SKIP2                                                                
331200     EVALUATE RID-IDPTYP                                                  
331300       WHEN 'RID'                                                         
331400         WRITE RID-POST-JP FROM UTKORT-LONG                               
331500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
331600       WHEN 'RIE'                                                         
331700         WRITE RIE-POST-JP FROM UTKORT-LONG                               
331800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
331900       WHEN 'RIH'                                                         
332000         WRITE RIH-POST-JP FROM UTKORT-LONG                               
332100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
332200       WHEN 'RIO'                                                         
332300         WRITE SOFT-RIO-POST-JP FROM UTKORT-LONG                          
332400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
332500       WHEN 'RKB'                                                         
332600         WRITE RKB-POST-JP FROM UTKORT-LONG                               
332700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
332800       WHEN 'RKC'                                                         
332900         WRITE RKC-POST-JP FROM UTKORT-LONG                               
333000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
333100       WHEN 'RKD'                                                         
333200         WRITE RKD-POST-JP FROM UTKORT-LONG                               
333300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
333400       WHEN OTHER                                                         
333500         WRITE IMP-JP FROM UTKORT                                         
333600         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
333700     END-EVALUATE                                                         
333800     MOVE 'W4612H'               TO POSTSUM-FDNAMN                        
333900     MOVE 'W46120DP'             TO POSTSUM-DDNAMN2                       
334000     CALL POSTSUM      USING POSTSUM-PARM                                 
334100     .                                                                    
334200     EJECT                                                                
334300 S10-SKRIV-W4612J SECTION.                                                
334400     SKIP2                                                                
334500     EVALUATE RID-IDPTYP                                                  
334600       WHEN 'RID'                                                         
334700         WRITE RID-POST-TH FROM UTKORT-LONG                               
334800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
334900       WHEN 'RIE'                                                         
335000         WRITE RIE-POST-TH FROM UTKORT-LONG                               
335100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
335200       WHEN 'RIH'                                                         
335300         WRITE RIH-POST-TH FROM UTKORT-LONG                               
335400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
335500       WHEN 'RIO'                                                         
335600         WRITE SOFT-RIO-POST-TH FROM UTKORT-LONG                          
335700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
335800       WHEN 'RKB'                                                         
335900         WRITE RKB-POST-TH FROM UTKORT-LONG                               
336000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
336100       WHEN 'RKC'                                                         
336200         WRITE RKC-POST-TH FROM UTKORT-LONG                               
336300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
336400       WHEN 'RKD'                                                         
336500         WRITE RKD-POST-TH FROM UTKORT-LONG                               
336600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
336700       WHEN OTHER                                                         
336800         WRITE IMP-TH FROM UTKORT                                         
336900         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
337000     END-EVALUATE                                                         
337100     MOVE 'W4612J'               TO POSTSUM-FDNAMN                        
337200     MOVE 'W46120E2'             TO POSTSUM-DDNAMN2                       
337300     CALL POSTSUM      USING POSTSUM-PARM                                 
337400     .                                                                    
337500     EJECT                                                                
337600 S10-SKRIV-W4612I SECTION.                                                
337700     SKIP2                                                                
337800     EVALUATE RID-IDPTYP                                                  
337900       WHEN 'RID'                                                         
338000         WRITE RID-POST-MY FROM UTKORT-LONG                               
338100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
338200       WHEN 'RIE'                                                         
338300         WRITE RIE-POST-MY FROM UTKORT-LONG                               
338400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
338500       WHEN 'RIH'                                                         
338600         WRITE RIH-POST-MY FROM UTKORT-LONG                               
338700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
338800       WHEN 'RIO'                                                         
338900         WRITE SOFT-RIO-POST-MY FROM UTKORT-LONG                          
339000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
339100       WHEN 'RKB'                                                         
339200         WRITE RKB-POST-MY FROM UTKORT-LONG                               
339300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
339400       WHEN 'RKC'                                                         
339500         WRITE RKC-POST-MY FROM UTKORT-LONG                               
339600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
339700       WHEN 'RKD'                                                         
339800         WRITE RKD-POST-MY FROM UTKORT-LONG                               
339900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
340000       WHEN OTHER                                                         
340100         WRITE IMP-MY FROM UTKORT                                         
340200         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
340300     END-EVALUATE                                                         
340400     MOVE 'W4612I'               TO POSTSUM-FDNAMN                        
340500     MOVE 'W46120E1'             TO POSTSUM-DDNAMN2                       
340600     CALL POSTSUM      USING POSTSUM-PARM                                 
340700     .                                                                    
340800     EJECT                                                                
340900 S10-SKRIV-W46128 SECTION.                                                
341000     SKIP2                                                                
341100     EVALUATE RID-IDPTYP                                                  
341200       WHEN 'RID'                                                         
341300         WRITE RID-POST-JP5220 FROM UTKORT-LONG                           
341400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
341500       WHEN 'RIE'                                                         
341600         WRITE RIE-POST-JP5220 FROM UTKORT-LONG                           
341700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
341800       WHEN 'RIH'                                                         
341900         WRITE RIH-POST-JP5220 FROM UTKORT-LONG                           
342000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
342100       WHEN 'RIO'                                                         
342200         WRITE SOFT-RIO-POST-JP5220 FROM UTKORT-LONG                      
342300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
342400       WHEN 'RKB'                                                         
342500         WRITE RKB-POST-JP5220 FROM UTKORT-LONG                           
342600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
342700       WHEN 'RKC'                                                         
342800         WRITE RKC-POST-JP5220 FROM UTKORT-LONG                           
342900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
343000       WHEN 'RKD'                                                         
343100         WRITE RKD-POST-JP5220 FROM UTKORT-LONG                           
343200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
343300       WHEN OTHER                                                         
343400         WRITE IMP-JP5220 FROM UTKORT                                     
343500         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
343600     END-EVALUATE                                                         
343700     MOVE 'W46128'               TO POSTSUM-FDNAMN                        
343800     MOVE 'W46120DS'             TO POSTSUM-DDNAMN2                       
343900     CALL POSTSUM      USING POSTSUM-PARM                                 
344000     .                                                                    
344100     EJECT                                                                
344200 S10-SKRIV-W4612B SECTION.                                                
344300     SKIP2                                                                
344400     EVALUATE RID-IDPTYP                                                  
344500       WHEN 'RID'                                                         
344600         WRITE RID-POST-GB1378 FROM UTKORT-LONG                           
344700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
344800       WHEN 'RIE'                                                         
344900         WRITE RIE-POST-GB1378 FROM UTKORT-LONG                           
345000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
345100       WHEN 'RIH'                                                         
345200         WRITE RIH-POST-GB1378 FROM UTKORT-LONG                           
345300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
345400       WHEN 'RIO'                                                         
345500         WRITE SOFT-RIO-POST-GB1378 FROM UTKORT-LONG                      
345600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
345700       WHEN 'RKB'                                                         
345800         WRITE RKB-POST-GB1378 FROM UTKORT-LONG                           
345900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
346000       WHEN 'RKC'                                                         
346100         WRITE RKC-POST-GB1378 FROM UTKORT-LONG                           
346200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
346300       WHEN 'RKD'                                                         
346400         WRITE RKD-POST-GB1378 FROM UTKORT-LONG                           
346500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
346600       WHEN OTHER                                                         
346700         WRITE IMP-GB1378 FROM UTKORT                                     
346800         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
346900     END-EVALUATE                                                         
347000     MOVE 'W4612B'               TO POSTSUM-FDNAMN                        
347100     MOVE 'W46120DT'             TO POSTSUM-DDNAMN2                       
347200     CALL POSTSUM      USING POSTSUM-PARM                                 
347300     .                                                                    
347400     EJECT                                                                
347500 S10-SKRIV-W4612C SECTION.                                                
347600     SKIP2                                                                
347700     EVALUATE RID-IDPTYP                                                  
347800       WHEN 'RID'                                                         
347900         WRITE RID-POST-PL FROM UTKORT-LONG                               
348000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
348100       WHEN 'RIE'                                                         
348200         WRITE RIE-POST-PL FROM UTKORT-LONG                               
348300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
348400       WHEN 'RIH'                                                         
348500         WRITE RIH-POST-PL FROM UTKORT-LONG                               
348600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
348700       WHEN 'RIO'                                                         
348800         WRITE SOFT-RIO-POST-PL FROM UTKORT-LONG                          
348900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
349000       WHEN 'RKB'                                                         
349100         WRITE RKB-POST-PL FROM UTKORT-LONG                               
349200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
349300       WHEN 'RKC'                                                         
349400         WRITE RKC-POST-PL FROM UTKORT-LONG                               
349500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
349600       WHEN 'RKD'                                                         
349700         WRITE RKD-POST-PL FROM UTKORT-LONG                               
349800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
349900       WHEN OTHER                                                         
350000         WRITE IMP-PL FROM UTKORT                                         
350100         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
350200     END-EVALUATE                                                         
350300     MOVE 'W4612C'               TO POSTSUM-FDNAMN                        
350400     MOVE 'W46120DU'             TO POSTSUM-DDNAMN2                       
350500     CALL POSTSUM      USING POSTSUM-PARM                                 
350600     .                                                                    
350700     EJECT                                                                
350800 S10-SKRIV-W4612K SECTION.                                                
350900     SKIP2                                                                
351000     EVALUATE RID-IDPTYP                                                  
351100       WHEN 'RID'                                                         
351200         WRITE RID-POST-IE     FROM UTKORT-LONG                           
351300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
351400       WHEN 'RIE'                                                         
351500         WRITE RIE-POST-IE     FROM UTKORT-LONG                           
351600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
351700       WHEN 'RIH'                                                         
351800         WRITE RIH-POST-IE     FROM UTKORT-LONG                           
351900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
352000       WHEN 'RIO'                                                         
352100         WRITE SOFT-RIO-POST-IE     FROM UTKORT-LONG                      
352200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
352300       WHEN 'RKB'                                                         
352400         WRITE RKB-POST-IE FROM UTKORT-LONG                               
352500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
352600       WHEN 'RKC'                                                         
352700         WRITE RKC-POST-IE FROM UTKORT-LONG                               
352800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
352900       WHEN 'RKD'                                                         
353000         WRITE RKD-POST-IE FROM UTKORT-LONG                               
353100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
353200       WHEN OTHER                                                         
353300         WRITE IMP-IE     FROM UTKORT                                     
353400         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
353500     END-EVALUATE                                                         
353600     MOVE 'W4612K'               TO POSTSUM-FDNAMN                        
353700     MOVE 'W46120E3'             TO POSTSUM-DDNAMN2                       
353800     CALL POSTSUM      USING POSTSUM-PARM                                 
353900     .                                                                    
354000     EJECT                                                                
354100 S10-SKRIV-W4612L SECTION.                                                
354200     SKIP2                                                                
354300     EVALUATE RID-IDPTYP                                                  
354400       WHEN 'RID'                                                         
354500         WRITE RID-POST-BR-NEW FROM UTKORT-LONG                           
354600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
354700       WHEN 'RIE'                                                         
354800         WRITE RIE-POST-BR-NEW FROM UTKORT-LONG                           
354900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
355000       WHEN 'RIH'                                                         
355100         WRITE RIH-POST-BR-NEW FROM UTKORT-LONG                           
355200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
355300       WHEN 'RIO'                                                         
355400         WRITE SOFT-RIO-POST-BR-NEW FROM UTKORT-LONG                      
355500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
355600       WHEN 'RKB'                                                         
355700         WRITE RKB-POST-BR-NEW FROM UTKORT-LONG                           
355800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
355900       WHEN 'RKC'                                                         
356000         WRITE RKC-POST-BR-NEW FROM UTKORT-LONG                           
356100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
356200       WHEN 'RKD'                                                         
356300         WRITE RKD-POST-BR-NEW FROM UTKORT-LONG                           
356400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
356500       WHEN OTHER                                                         
356600         WRITE IMP-BR-NEW FROM UTKORT                                     
356700         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
356800     END-EVALUATE                                                         
356900     MOVE 'W4612L'               TO POSTSUM-FDNAMN                        
357000     MOVE 'W46120D7'             TO POSTSUM-DDNAMN2                       
357100     CALL POSTSUM      USING POSTSUM-PARM                                 
357200     .                                                                    
357300     EJECT                                                                
357400 S10-SKRIV-W4612M SECTION.                                                
357500     SKIP2                                                                
357600     EVALUATE RID-IDPTYP                                                  
357700       WHEN 'RID'                                                         
357800         WRITE RID-POST-MX     FROM UTKORT-LONG                           
357900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
358000       WHEN 'RIE'                                                         
358100         WRITE RIE-POST-MX     FROM UTKORT-LONG                           
358200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
358300       WHEN 'RIH'                                                         
358400         WRITE RIH-POST-MX     FROM UTKORT-LONG                           
358500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
358600       WHEN 'RIO'                                                         
358700         WRITE SOFT-RIO-POST-MX     FROM UTKORT-LONG                      
358800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
358900       WHEN 'RKB'                                                         
359000         WRITE RKB-POST-MX FROM UTKORT-LONG                               
359100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
359200       WHEN 'RKC'                                                         
359300         WRITE RKC-POST-MX FROM UTKORT-LONG                               
359400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
359500       WHEN 'RKD'                                                         
359600         WRITE RKD-POST-MX FROM UTKORT-LONG                               
359700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
359800       WHEN OTHER                                                         
359900         WRITE IMP-MX     FROM UTKORT                                     
360000         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
360100     END-EVALUATE                                                         
360200     MOVE 'W4612M'               TO POSTSUM-FDNAMN                        
360300     MOVE 'W46120DZ'             TO POSTSUM-DDNAMN2                       
360400     CALL POSTSUM      USING POSTSUM-PARM                                 
360500     .                                                                    
360600 S10-SKRIV-W4612N SECTION.                                                
360700     SKIP2                                                                
360800     EVALUATE RID-IDPTYP                                                  
360900       WHEN 'RID'                                                         
361000         WRITE RID-POST-TR     FROM UTKORT-LONG                           
361100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
361200       WHEN 'RIE'                                                         
361300         WRITE RIE-POST-TR     FROM UTKORT-LONG                           
361400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
361500       WHEN 'RIH'                                                         
361600         WRITE RIH-POST-TR     FROM UTKORT-LONG                           
361700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
361800       WHEN 'RIO'                                                         
361900         WRITE SOFT-RIO-POST-TR     FROM UTKORT-LONG                      
362000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
362100       WHEN 'RKB'                                                         
362200         WRITE RKB-POST-TR FROM UTKORT-LONG                               
362300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
362400       WHEN 'RKC'                                                         
362500         WRITE RKC-POST-TR FROM UTKORT-LONG                               
362600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
362700       WHEN 'RKD'                                                         
362800         WRITE RKD-POST-TR FROM UTKORT-LONG                               
362900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
363000       WHEN OTHER                                                         
363100         WRITE IMP-TR     FROM UTKORT                                     
363200         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
363300     END-EVALUATE                                                         
363400     MOVE 'W4612N'               TO POSTSUM-FDNAMN                        
363500     MOVE 'W46120DZ'             TO POSTSUM-DDNAMN2                       
363600     CALL POSTSUM      USING POSTSUM-PARM                                 
363700     .                                                                    
363800     EJECT                                                                
363900 S10-SKRIV-W4612Z SECTION.                                                
364000     SKIP2                                                                
364100     EVALUATE RID-IDPTYP                                                  
364200       WHEN 'RID'                                                         
364300         WRITE RID-POST-FI1091 FROM UTKORT-LONG                           
364400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
364500       WHEN 'RIE'                                                         
364600         WRITE RIE-POST-FI1091 FROM UTKORT-LONG                           
364700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
364800       WHEN 'RIH'                                                         
364900         WRITE RIH-POST-FI1091 FROM UTKORT-LONG                           
365000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
365100       WHEN 'RIO'                                                         
365200         WRITE SOFT-RIO-POST-FI1091 FROM UTKORT-LONG                      
365300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
365400       WHEN 'RKB'                                                         
365500         WRITE RKB-POST-FI1091 FROM UTKORT-LONG                           
365600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
365700       WHEN 'RKC'                                                         
365800         WRITE RKC-POST-FI1091 FROM UTKORT-LONG                           
365900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
366000       WHEN 'RKD'                                                         
366100         WRITE RKD-POST-FI1091 FROM UTKORT-LONG                           
366200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
366300       WHEN OTHER                                                         
366400         WRITE IMP-FI1091 FROM UTKORT                                     
366500         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
366600     END-EVALUATE                                                         
366700     MOVE 'W4612Z'               TO POSTSUM-FDNAMN                        
366800     MOVE 'W46120DN'             TO POSTSUM-DDNAMN2                       
366900     CALL POSTSUM      USING POSTSUM-PARM                                 
367000     .                                                                    
367100     EJECT                                                                
367200 S10-SKRIV-W4612D SECTION.                                                
367300     SKIP2                                                                
367400     EVALUATE RID-IDPTYP                                                  
367500       WHEN 'RID'                                                         
367600         WRITE RID-POST-CAN FROM UTKORT-LONG                              
367700         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
367800       WHEN 'RIE'                                                         
367900         WRITE RIE-POST-CAN FROM UTKORT-LONG                              
368000         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
368100       WHEN 'RIH'                                                         
368200         WRITE RIH-POST-CAN FROM UTKORT-LONG                              
368300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
368400       WHEN 'RIO'                                                         
368500         WRITE SOFT-RIO-POST-CAN FROM UTKORT-LONG                         
368600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
368700       WHEN 'RKB'                                                         
368800         WRITE RKB-POST-CAN FROM UTKORT-LONG                              
368900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
369000       WHEN 'RKC'                                                         
369100         WRITE RKC-POST-CAN FROM UTKORT-LONG                              
369200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
369300       WHEN 'RKD'                                                         
369400         WRITE RKD-POST-CAN FROM UTKORT-LONG                              
369500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
369600       WHEN OTHER                                                         
369700         WRITE IMP-CAN FROM UTKORT                                        
369800         MOVE RIA-IDPTYP     TO POSTSUM-TRANSTYP                          
369900     END-EVALUATE                                                         
370000                                                                          
370100     MOVE 'W4612D'               TO POSTSUM-FDNAMN                        
370200     MOVE 'W46120DV'             TO POSTSUM-DDNAMN2                       
370300     CALL POSTSUM      USING POSTSUM-PARM                                 
370400     .                                                                    
370500     EJECT                                                                
370600 S10-SKRIV-W4612E SECTION.                                                
370700     SKIP2                                                                
370800     EVALUATE RID-IDPTYP                                                  
370900       WHEN 'RID'                                                         
371000         WRITE RID-POST-USA FROM UTKORT-LONG                              
371100         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
371200       WHEN 'RIE'                                                         
371300         WRITE RIE-POST-USA FROM UTKORT-LONG                              
371400         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
371500       WHEN 'RIH'                                                         
371600         WRITE RIH-POST-USA FROM UTKORT-LONG                              
371700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
371800       WHEN 'RIO'                                                         
371900         WRITE SOFT-RIO-POST-USA FROM UTKORT-LONG                         
372000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
372100       WHEN 'RKB'                                                         
372200         WRITE RKB-POST-USA FROM UTKORT-LONG                              
372300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
372400       WHEN 'RKC'                                                         
372500         WRITE RKC-POST-USA FROM UTKORT-LONG                              
372600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
372700       WHEN 'RKD'                                                         
372800         WRITE RKD-POST-USA FROM UTKORT-LONG                              
372900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
373000       WHEN OTHER                                                         
373100         WRITE IMP-USA FROM UTKORT                                        
373200         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
373300     END-EVALUATE                                                         
373400     MOVE 'W4612E'               TO POSTSUM-FDNAMN                        
373500     MOVE 'W46120DW'             TO POSTSUM-DDNAMN2                       
373600     CALL POSTSUM      USING POSTSUM-PARM                                 
373700     .                                                                    
373800     EJECT                                                                
373900 S10-SKRIV-W4612F SECTION.                                                
374000     SKIP2                                                                
374100     EVALUATE RID-IDPTYP                                                  
374200       WHEN 'RID'                                                         
374300         WRITE RID-POST-KR  FROM UTKORT-LONG                              
374400         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
374500       WHEN 'RIE'                                                         
374600         WRITE RIE-POST-KR FROM UTKORT-LONG                               
374700         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
374800       WHEN 'RIH'                                                         
374900         WRITE RIH-POST-KR FROM UTKORT-LONG                               
375000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
375100       WHEN 'RIO'                                                         
375200         WRITE SOFT-RIO-POST-KR FROM UTKORT-LONG                          
375300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
375400       WHEN 'RKB'                                                         
375500         WRITE RKB-POST-KR FROM UTKORT-LONG                               
375600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
375700       WHEN 'RKC'                                                         
375800         WRITE RKC-POST-KR FROM UTKORT-LONG                               
375900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
376000       WHEN 'RKD'                                                         
376100         WRITE RKD-POST-KR FROM UTKORT-LONG                               
376200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
376300       WHEN OTHER                                                         
376400         WRITE IMP-KR FROM UTKORT                                         
376500         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
376600     END-EVALUATE                                                         
376700     MOVE 'W4612F'               TO POSTSUM-FDNAMN                        
376800     MOVE 'W46120DX'             TO POSTSUM-DDNAMN2                       
376900     CALL POSTSUM      USING POSTSUM-PARM                                 
377000     .                                                                    
377100     EJECT                                                                
377200 S10-SKRIV-W4612G SECTION.                                                
377300     SKIP2                                                                
377400     EVALUATE RID-IDPTYP                                                  
377500       WHEN 'RID'                                                         
377600         WRITE RID-POST-PT  FROM UTKORT-LONG                              
377700         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
377800       WHEN 'RIE'                                                         
377900         WRITE RIE-POST-PT FROM UTKORT-LONG                               
378000         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
378100       WHEN 'RIH'                                                         
378200         WRITE RIH-POST-PT FROM UTKORT-LONG                               
378300         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
378400       WHEN 'RIO'                                                         
378500         WRITE SOFT-RIO-POST-PT FROM UTKORT-LONG                          
378600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
378700       WHEN 'RKB'                                                         
378800         WRITE RKB-POST-PT FROM UTKORT-LONG                               
378900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
379000       WHEN 'RKC'                                                         
379100         WRITE RKC-POST-PT FROM UTKORT-LONG                               
379200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
379300       WHEN 'RKD'                                                         
379400         WRITE RKD-POST-PT FROM UTKORT-LONG                               
379500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
379600       WHEN OTHER                                                         
379700         WRITE IMP-PT FROM UTKORT                                         
379800         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
379900     END-EVALUATE                                                         
380000     MOVE 'W4612G'               TO POSTSUM-FDNAMN                        
380100     MOVE 'W46120DY'             TO POSTSUM-DDNAMN2                       
380200     CALL POSTSUM      USING POSTSUM-PARM                                 
380300     .                                                                    
380400     EJECT                                                                
380500 S10-SKRIV-W4612P SECTION.                                                
380600     SKIP2                                                                
380700     EVALUATE RID-IDPTYP                                                  
380800       WHEN 'RID'                                                         
380900         WRITE RID-POST-RU  FROM UTKORT-LONG                              
381000         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
381100       WHEN 'RIE'                                                         
381200         WRITE RIE-POST-RU FROM UTKORT-LONG                               
381300         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
381400       WHEN 'RIH'                                                         
381500         WRITE RIH-POST-RU FROM UTKORT-LONG                               
381600         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
381700       WHEN 'RIO'                                                         
381800         WRITE SOFT-RIO-POST-RU FROM UTKORT-LONG                          
381900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
382000       WHEN 'RKB'                                                         
382100         WRITE RKB-POST-RU FROM UTKORT-LONG                               
382200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
382300       WHEN 'RKC'                                                         
382400         WRITE RKC-POST-RU FROM UTKORT-LONG                               
382500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
382600       WHEN 'RKD'                                                         
382700         WRITE RKD-POST-RU FROM UTKORT-LONG                               
382800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
382900       WHEN OTHER                                                         
383000         WRITE IMP-RU FROM UTKORT                                         
383100         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
383200     END-EVALUATE                                                         
383300     MOVE 'W4612P'               TO POSTSUM-FDNAMN                        
383400     MOVE 'W46120E5'             TO POSTSUM-DDNAMN2                       
383500     CALL POSTSUM      USING POSTSUM-PARM                                 
383600     .                                                                    
383700     EJECT                                                                
383800 S10-SKRIV-W461CN SECTION.                                                
383900     SKIP2                                                                
384000     EVALUATE RID-IDPTYP                                                  
384100       WHEN 'RID'                                                         
384200         WRITE RID-POST-CN  FROM UTKORT-LONG                              
384300         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
384400       WHEN 'RIE'                                                         
384500         WRITE RIE-POST-CN FROM UTKORT-LONG                               
384600         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
384700       WHEN 'RIH'                                                         
384800         WRITE RIH-POST-CN FROM UTKORT-LONG                               
384900         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
385000       WHEN 'RIO'                                                         
385100         WRITE SOFT-RIO-POST-CN FROM UTKORT-LONG                          
385200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
385300       WHEN 'RKB'                                                         
385400         WRITE RKB-POST-CN FROM UTKORT-LONG                               
385500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
385600       WHEN 'RKC'                                                         
385700         WRITE RKC-POST-CN FROM UTKORT-LONG                               
385800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
385900       WHEN 'RKD'                                                         
386000         WRITE RKD-POST-CN FROM UTKORT-LONG                               
386100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
386200       WHEN OTHER                                                         
386300         WRITE IMP-CN FROM UTKORT                                         
386400         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
386500     END-EVALUATE                                                         
386600     MOVE 'W461CN'               TO POSTSUM-FDNAMN                        
386700     MOVE 'W46120E6'             TO POSTSUM-DDNAMN2                       
386800     CALL POSTSUM      USING POSTSUM-PARM                                 
386900     .                                                                    
387000     EJECT                                                                
387100 S10-SKRIV-W461CN1 SECTION.                                               
387200     SKIP2                                                                
387300     EVALUATE RID-IDPTYP                                                  
387400       WHEN 'RID'                                                         
387500         WRITE RID-POST-CN1 FROM UTKORT-LONG                              
387600         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
387700       WHEN 'RIE'                                                         
387800         WRITE RIE-POST-CN1 FROM UTKORT-LONG                              
387900         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
388000       WHEN 'RIH'                                                         
388100         WRITE RIH-POST-CN1 FROM UTKORT-LONG                              
388200         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
388300       WHEN 'RIO'                                                         
388400         WRITE SOFT-RIO-POST-CN1 FROM UTKORT-LONG                         
388500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
388600       WHEN 'RKB'                                                         
388700         WRITE RKB-POST-CN1 FROM UTKORT-LONG                              
388800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
388900       WHEN 'RKC'                                                         
389000         WRITE RKC-POST-CN1 FROM UTKORT-LONG                              
389100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
389200       WHEN 'RKD'                                                         
389300         WRITE RKD-POST-CN1 FROM UTKORT-LONG                              
389400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
389500       WHEN OTHER                                                         
389600         WRITE IMP-CN1 FROM UTKORT                                        
389700         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
389800     END-EVALUATE                                                         
389900     MOVE 'W461CN1'              TO POSTSUM-FDNAMN                        
390000     MOVE 'W46120E9'             TO POSTSUM-DDNAMN2                       
390100     CALL POSTSUM      USING POSTSUM-PARM                                 
390200     .                                                                    
390300     EJECT                                                                
390400 S10-SKRIV-W461ZA SECTION.                                                
390500     SKIP2                                                                
390600     EVALUATE RID-IDPTYP                                                  
390700       WHEN 'RID'                                                         
390800         WRITE RID-POST-ZA  FROM UTKORT-LONG                              
390900         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
391000       WHEN 'RIE'                                                         
391100         WRITE RIE-POST-ZA FROM UTKORT-LONG                               
391200         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
391300       WHEN 'RIH'                                                         
391400         WRITE RIH-POST-ZA FROM UTKORT-LONG                               
391500         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
391600       WHEN 'RIO'                                                         
391700         WRITE SOFT-RIO-POST-ZA FROM UTKORT-LONG                          
391800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
391900       WHEN 'RKB'                                                         
392000         WRITE RKB-POST-ZA FROM UTKORT-LONG                               
392100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
392200       WHEN 'RKC'                                                         
392300         WRITE RKC-POST-ZA FROM UTKORT-LONG                               
392400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
392500       WHEN 'RKD'                                                         
392600         WRITE RKD-POST-ZA FROM UTKORT-LONG                               
392700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
392800       WHEN OTHER                                                         
392900         WRITE IMP-ZA FROM UTKORT                                         
393000         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
393100     END-EVALUATE                                                         
393200     MOVE 'W461ZA'               TO POSTSUM-FDNAMN                        
393300     MOVE 'W46120E7'             TO POSTSUM-DDNAMN2                       
393400     CALL POSTSUM      USING POSTSUM-PARM                                 
393500     .                                                                    
393600     EJECT                                                                
393700 S10-SKRIV-W461PT3 SECTION.                                               
393800     SKIP2                                                                
393900     EVALUATE RID-IDPTYP                                                  
394000       WHEN 'RID'                                                         
394100         WRITE RID-POST-PT3 FROM UTKORT-LONG                              
394200         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
394300       WHEN 'RIE'                                                         
394400         WRITE RIE-POST-PT3 FROM UTKORT-LONG                              
394500         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
394600       WHEN 'RIH'                                                         
394700         WRITE RIH-POST-PT3 FROM UTKORT-LONG                              
394800         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
394900       WHEN 'RIO'                                                         
395000         WRITE SOFT-RIO-POST-PT3 FROM UTKORT-LONG                         
395100         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
395200       WHEN 'RKB'                                                         
395300         WRITE RKB-POST-PT3 FROM UTKORT-LONG                              
395400         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
395500       WHEN 'RKC'                                                         
395600         WRITE RKC-POST-PT3 FROM UTKORT-LONG                              
395700         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
395800       WHEN 'RKD'                                                         
395900         WRITE RKD-POST-PT3 FROM UTKORT-LONG                              
396000         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
396100       WHEN OTHER                                                         
396200         WRITE IMP-PT3 FROM UTKORT                                        
396300         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
396400     END-EVALUATE                                                         
396500     MOVE 'W461PT3'              TO POSTSUM-FDNAMN                        
396600     MOVE 'W46120E8'             TO POSTSUM-DDNAMN2                       
396700     CALL POSTSUM      USING POSTSUM-PARM                                 
396800     .                                                                    
396901     EJECT                                                                
397001 S10-SKRIV-W46126 SECTION.                                                
397101     SKIP2                                                                
397201     EVALUATE RID-IDPTYP                                                  
397301       WHEN 'RID'                                                         
397401         WRITE RID-POST-IN FROM UTKORT-LONG                               
397501         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
397601       WHEN 'RIE'                                                         
397701         WRITE RIE-POST-IN FROM UTKORT-LONG                               
397801         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
397901       WHEN 'RIH'                                                         
398001         WRITE RIH-POST-IN FROM UTKORT-LONG                               
398101         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
398201       WHEN 'RIO'                                                         
398301         WRITE SOFT-RIO-POST-IN FROM UTKORT-LONG                          
398401         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
398501       WHEN 'RKB'                                                         
398601         WRITE RKB-POST-IN FROM UTKORT-LONG                               
398701         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
398801       WHEN 'RKC'                                                         
398901         WRITE RKC-POST-IN FROM UTKORT-LONG                               
399001         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
399101       WHEN 'RKD'                                                         
399201         WRITE RKD-POST-IN FROM UTKORT-LONG                               
399301         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
399401       WHEN OTHER                                                         
399501         WRITE IMP-IN FROM UTKORT                                         
399601         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
399701     END-EVALUATE                                                         
399801     MOVE 'W46126'               TO POSTSUM-FDNAMN                        
399901     MOVE 'W46120EA'             TO POSTSUM-DDNAMN2                       
400001     CALL POSTSUM      USING POSTSUM-PARM                                 
400101     .                                                                    
400201 S10-SKRIV-W46122 SECTION.                                                
400301     SKIP2                                                                
400401     EVALUATE RID-IDPTYP                                                  
400501       WHEN 'RID'                                                         
400601         WRITE RID-POST-CZ FROM UTKORT-LONG                               
400701         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
400801       WHEN 'RIE'                                                         
400901         WRITE RIE-POST-CZ FROM UTKORT-LONG                               
401001         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
401101       WHEN 'RIH'                                                         
401201         WRITE RIH-POST-CZ FROM UTKORT-LONG                               
401301         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
401401       WHEN 'RIO'                                                         
401501         WRITE SOFT-RIO-POST-CZ FROM UTKORT-LONG                          
401601         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
401701       WHEN 'RKB'                                                         
401801         WRITE RKB-POST-CZ FROM UTKORT-LONG                               
401901         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
402001       WHEN 'RKC'                                                         
402101         WRITE RKC-POST-CZ FROM UTKORT-LONG                               
402201         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
402301       WHEN 'RKD'                                                         
402401         WRITE RKD-POST-CZ FROM UTKORT-LONG                               
402501         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
402601       WHEN OTHER                                                         
402701         WRITE IMP-CZ FROM UTKORT                                         
402801         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
402901     END-EVALUATE                                                         
403001     MOVE 'W46122'               TO POSTSUM-FDNAMN                        
403101     MOVE 'W46120EB'             TO POSTSUM-DDNAMN2                       
403201     CALL POSTSUM      USING POSTSUM-PARM                                 
403301     .                                                                    
403401 S10-SKRIV-W46123 SECTION.                                                
403501     SKIP2                                                                
403601     EVALUATE RID-IDPTYP                                                  
403701       WHEN 'RID'                                                         
403801         WRITE RID-POST-HU FROM UTKORT-LONG                               
403901         MOVE RID-IDPTYP    TO POSTSUM-TRANSTYP                           
404001       WHEN 'RIE'                                                         
404101         WRITE RIE-POST-HU FROM UTKORT-LONG                               
404201         MOVE RID-IDPTYP  TO POSTSUM-TRANSTYP                             
404301       WHEN 'RIH'                                                         
404401         WRITE RIH-POST-HU FROM UTKORT-LONG                               
404501         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
404601       WHEN 'RIO'                                                         
404701         WRITE SOFT-RIO-POST-HU FROM UTKORT-LONG                          
404801         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
404901       WHEN 'RKB'                                                         
405001         WRITE RKB-POST-HU FROM UTKORT-LONG                               
405101         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
405201       WHEN 'RKC'                                                         
405301         WRITE RKC-POST-HU FROM UTKORT-LONG                               
405401         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
405501       WHEN 'RKD'                                                         
405601         WRITE RKD-POST-HU FROM UTKORT-LONG                               
405701         MOVE RID-IDPTYP TO POSTSUM-TRANSTYP                              
405801       WHEN OTHER                                                         
405901         WRITE IMP-HU FROM UTKORT                                         
406001         MOVE RIA-IDPTYP TO POSTSUM-TRANSTYP                              
406101     END-EVALUATE                                                         
406201     MOVE 'W46123'               TO POSTSUM-FDNAMN                        
406301     MOVE 'W46120EC'             TO POSTSUM-DDNAMN2                       
406401     CALL POSTSUM      USING POSTSUM-PARM                                 
406501     .                                                                    
406601     EJECT                                                                
406700 S20-SKRIV-OBHUV SECTION.                                                 
406800     SKIP2                                                                
406900     WRITE OBHUV-POST FROM W46120-AREA                                    
407000     MOVE SPACE TO W46120-AREA                                            
407100     MOVE 'W46120'               TO POSTSUM-FDNAMN                        
407200     MOVE 'W46120D2'             TO POSTSUM-DDNAMN2                       
407300     MOVE W46120-OBHUV-IDPTYP    TO POSTSUM-TRANSTYP                      
407400     CALL POSTSUM      USING POSTSUM-PARM                                 
407500     .                                                                    
407600     EJECT                                                                
407700 S21-SKRIV-OBEN SECTION.                                                  
407800     SKIP2                                                                
407900     WRITE OBEN-POST FROM W46120-AREA                                     
408000     MOVE 'W46120'               TO POSTSUM-FDNAMN                        
408100     MOVE 'W46120D2'             TO POSTSUM-DDNAMN2                       
408200     MOVE W46120-OBEN-IDPTYP     TO POSTSUM-TRANSTYP                      
408300     CALL POSTSUM      USING POSTSUM-PARM                                 
408400     .                                                                    
408500     EJECT                                                                
408600 S22-SKRIV-OBEEN SECTION.                                                 
408700     SKIP2                                                                
408800     WRITE OBEEN-POST FROM W46120-AREA                                    
408900     MOVE 'W46120'               TO POSTSUM-FDNAMN                        
409000     MOVE 'W46120D2'             TO POSTSUM-DDNAMN2                       
409100     MOVE W46120-OBEEN-IDPTYP    TO POSTSUM-TRANSTYP                      
409200     CALL POSTSUM      USING POSTSUM-PARM                                 
409300     .                                                                    
409400     EJECT                                                                
409500 S23-SKRIV-OBTEXT SECTION.                                                
409600     SKIP2                                                                
409700     WRITE OBTEXT-POST FROM W46120-AREA                                   
409800     MOVE 'W46120'               TO POSTSUM-FDNAMN                        
409900     MOVE 'W46120D2'             TO POSTSUM-DDNAMN2                       
410000     MOVE W46120-OBTEXT-IDPTYP   TO POSTSUM-TRANSTYP                      
410100     CALL POSTSUM      USING POSTSUM-PARM                                 
410200     .                                                                    
410300     EJECT                                                                
410400 S24-SKRIV-OBKVAN SECTION.                                                
410500     SKIP2                                                                
410600     WRITE OBKVAN-POST FROM W46120-AREA                                   
410700     MOVE 'W46120'               TO POSTSUM-FDNAMN                        
410800     MOVE 'W46120D2'             TO POSTSUM-DDNAMN2                       
410900     MOVE W46120-OBKVAN-IDPTYP   TO POSTSUM-TRANSTYP                      
411000     CALL POSTSUM      USING POSTSUM-PARM                                 
411100     .                                                                    
411200     EJECT                                                                
411300 S25-SKRIV-OBLAG SECTION.                                                 
411400     SKIP2                                                                
411500     WRITE OBLAG-POST FROM W46120-AREA                                    
411600     MOVE 'W46120'               TO POSTSUM-FDNAMN                        
411700     MOVE 'W46120D2'             TO POSTSUM-DDNAMN2                       
411800     MOVE W46120-OBLAG-IDPTYP    TO POSTSUM-TRANSTYP                      
411900     CALL POSTSUM      USING POSTSUM-PARM                                 
412000     .                                                                    
412100     EJECT                                                                
412200 S26-SKRIV-OBSTOP SECTION.                                                
412300     SKIP2                                                                
412400     WRITE OBSTOP-POST FROM W46120-AREA                                   
412500     MOVE 'W46120'               TO POSTSUM-FDNAMN                        
412600     MOVE 'W46120D2'             TO POSTSUM-DDNAMN2                       
412700     MOVE W46120-OBSTOP-IDPTYP   TO POSTSUM-TRANSTYP                      
412800     CALL POSTSUM      USING POSTSUM-PARM                                 
412900     .                                                                    
413000     EJECT                                                                
413100 Z-FINIT SECTION.                                                         
413200     SKIP2                                                                
413300     CLOSE W46120                                                         
413400     W46127 W46128                                                        
413500     W46129 W46130 W4612Z                                                 
413600     W46138 W46131 W46172                                                 
413700     W46171 W46161 W46179 W46168                                          
413800     W46178 W46176                                                        
413900     W46173 W46170 W46140 W46139                                          
414000     W46174 W46175 W46180 W46134                                          
414100     W4612A W4612B W4612C W4612G                                          
414200     W4612D W4612E W4612F W4612H                                          
414300     W4612I W4612J W4612K W4612L W4612M W4612N                            
414400     W4612P W461CN W461ZA W461CN1                                         
414500     W461PT3                                                              
414601     W46126                                                               
414701     W46122 W46123                                                        
414800                                                                          
414900     MOVE 'S' TO POSTSUM-OPKOD                                            
415000     CALL POSTSUM USING POSTSUM-PARM                                      
416000     .                                                                    
