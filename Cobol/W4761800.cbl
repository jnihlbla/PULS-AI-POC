000100 ID  DIVISION.                                                            
000200 PROGRAM-ID.    W4761800.                                                 
000300*AUTHOR.        STINA MOGREN.                                             
000400*DATE-WRITTEN.  AUG 2002.                                                 
000500*                                                                         
000600*                                                                         
000700*    REMARKS.   VIPS INFO IMPORTER                                        
000800*    FUNKTION:  DELAR UPP INFILEN PÅ EN UTFIL PER LAND                    
000900*                                                                         
001000*    ABENDKODER:                                                          
001100*                                                                         
001200*        U0016    - OM RETURKOD FRÅN SORT                                 
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*- - - - - - - - - - - - INFIL:                                           
002100     SELECT INFIL                        ASSIGN TO UT-S-W47618D1.         
002200     SKIP2                                                                
002300*- - - - - - - - - - - - UTFILER:                                         
002400*                  FAKTURA-POSTER                                         
002500     SELECT W46127                       ASSIGN TO UT-S-W47618D3.         
002600     SELECT W46129                       ASSIGN TO UT-S-W47618D4.         
002700     SELECT W46130                       ASSIGN TO UT-S-W47618D5.         
002800     SELECT W46172                       ASSIGN TO UT-S-W47618D6.         
002900     SELECT W4612L                       ASSIGN TO UT-S-W47618D7.         
003000     SELECT W46138                       ASSIGN TO UT-S-W47618D8.         
003100     SELECT W46171                       ASSIGN TO UT-S-W47618D9.         
003200     SELECT W46161                       ASSIGN TO UT-S-W47618DA.         
003300     SELECT W46179                       ASSIGN TO UT-S-W47618DB.         
003400     SELECT W46168                       ASSIGN TO UT-S-W47618DC.         
003500     SELECT W46178                       ASSIGN TO UT-S-W47618DD.         
003600     SELECT W46176                       ASSIGN TO UT-S-W47618DE.         
003700     SELECT W46173                       ASSIGN TO UT-S-W47618DF.         
003800     SELECT W46170                       ASSIGN TO UT-S-W47618DG.         
003900     SELECT W46140                       ASSIGN TO UT-S-W47618DH.         
004000     SELECT W46139                       ASSIGN TO UT-S-W47618DI.         
004100     SELECT W46174                       ASSIGN TO UT-S-W47618DJ.         
004200     SELECT W46175                       ASSIGN TO UT-S-W47618DK.         
004300     SELECT W46131                       ASSIGN TO UT-S-W47618DL.         
004400     SELECT W46180                       ASSIGN TO UT-S-W47618DM.         
004500     SELECT W4612Z                       ASSIGN TO UT-S-W47618DN.         
004600     SELECT W46134                       ASSIGN TO UT-S-W47618DO.         
004700     SELECT W4612A                       ASSIGN TO UT-S-W47618DQ.         
004800     SELECT W4612H                       ASSIGN TO UT-S-W47618DP.         
004900     SELECT W46128                       ASSIGN TO UT-S-W47618DS.         
005000     SELECT W4612B                       ASSIGN TO UT-S-W47618DT.         
005100     SELECT W4612C                       ASSIGN TO UT-S-W47618DU.         
005200     SELECT W4612D                       ASSIGN TO UT-S-W47618DV.         
005300     SELECT W4612E                       ASSIGN TO UT-S-W47618DW.         
005400     SELECT W4612F                       ASSIGN TO UT-S-W47618DX.         
005500     SELECT W4612G                       ASSIGN TO UT-S-W47618DY.         
005600     SELECT W4612M                       ASSIGN TO UT-S-W47618DZ.         
005700     SELECT W4612I                       ASSIGN TO UT-S-W47618E1.         
005800     SELECT W4612J                       ASSIGN TO UT-S-W47618E2.         
005900     SELECT W4612K                       ASSIGN TO UT-S-W47618E3.         
006000     SELECT W4612N                       ASSIGN TO UT-S-W47618E4.         
006100     SELECT W461RU                       ASSIGN TO UT-S-W47618E5.         
006200     SELECT W461CN                       ASSIGN TO UT-S-W47618E6.         
006300     SELECT W461ZA                       ASSIGN TO UT-S-W47618E7.         
006400     SELECT W461PT2                      ASSIGN TO UT-S-W47618E8.         
006500     SELECT W461CN1                      ASSIGN TO UT-S-W47618E9.         
006600     SELECT W461IN                       ASSIGN TO UT-S-W47618EA.         
006700     SELECT W461CZ                       ASSIGN TO UT-S-W47618EB.         
006800     SELECT W461HU                       ASSIGN TO UT-S-W47618EC.         
006900     EJECT                                                                
007000 DATA DIVISION.                                                           
007100     SKIP2                                                                
007200 FILE SECTION.                                                            
007300     SKIP3                                                                
007400 FD  INFIL                                                                
007500     RECORDING       V                                                    
007600     BLOCK CONTAINS 0.                                                    
007700     SKIP2                                                                
007800*01  FILLER -COPY W461RIO2   -L.                                          
007900     SKIP2                                                                
008000 FD  W46127                                                               
008100     RECORDING       V                                                    
008200     BLOCK CONTAINS 0.                                                    
008300     SKIP2                                                                
008400 01  IMP-IT                   PIC X(80).                                  
008500     SKIP2                                                                
008600*01  RIO-POST-IT       -COPY W461RIO2 -PRE SOFT-  -L.                     
008700     EJECT                                                                
008800 FD  W46129                                                               
008900     RECORDING       V                                                    
009000     BLOCK CONTAINS 0.                                                    
009100     SKIP2                                                                
009200 01  IMP-FI                  PIC X(80).                                   
009300     SKIP2                                                                
009400*01  RIO-POST-FI       -COPY W461RIO2 -PRE SOFT-  -L.                     
009500     EJECT                                                                
009600 FD  W46130                                                               
009700     RECORDING       V                                                    
009800     BLOCK CONTAINS 0.                                                    
009900     SKIP2                                                                
010000 01  IMP-BE                   PIC X(80).                                  
010100*01  RIO-POST-BE       -COPY W461RIO2 -PRE SOFT-  -L.                     
010200     EJECT                                                                
010300 FD  W46172                                                               
010400     RECORDING       V                                                    
010500     BLOCK CONTAINS 0.                                                    
010600     SKIP2                                                                
010700 01  IMP-CH2078               PIC X(80).                                  
010800*01  RIO-POST-CH2078   -COPY W461RIO2 -PRE SOFT-  -L.                     
010900     EJECT                                                                
011000 FD  W46138                                                               
011100     RECORDING       V                                                    
011200     BLOCK CONTAINS 0.                                                    
011300     SKIP2                                                                
011400 01  IMP-US                   PIC X(80).                                  
011500     SKIP2                                                                
011600*01  RIO-POST-US       -COPY W461RIO2 -PRE SOFT-  -L.                     
011700     EJECT                                                                
011800 FD  W46171                                                               
011900     RECORDING       V                                                    
012000     BLOCK CONTAINS 0.                                                    
012100     SKIP2                                                                
012200 01  IMP-DE                   PIC X(80).                                  
012300     SKIP2                                                                
012400*01  RIO-POST-DE       -COPY W461RIO2 -PRE SOFT-  -L.                     
012500     EJECT                                                                
012600 FD  W46161                                                               
012700     RECORDING       V                                                    
012800     BLOCK CONTAINS 0.                                                    
012900     SKIP2                                                                
013000 01  IMP-NL                   PIC X(80).                                  
013100     SKIP2                                                                
013200*01  RIO-POST-NL       -COPY W461RIO2 -PRE SOFT-  -L.                     
013300     EJECT                                                                
013400 FD  W46179                                                               
013500     RECORDING       V                                                    
013600     BLOCK CONTAINS 0.                                                    
013700     SKIP2                                                                
013800 01  IMP-ES                   PIC X(80).                                  
013900     SKIP2                                                                
014000*01  RIO-POST-ES       -COPY W461RIO2 -PRE SOFT-  -L.                     
014100     EJECT                                                                
014200 FD  W46168                                                               
014300     RECORDING       V                                                    
014400     BLOCK CONTAINS 0.                                                    
014500     SKIP2                                                                
014600 01  IMP-AT                   PIC X(80).                                  
014700     SKIP2                                                                
014800*01  RIO-POST-AT       -COPY W461RIO2 -PRE SOFT-  -L.                     
014900     EJECT                                                                
015000 FD  W46178                                                               
015100     RECORDING       V                                                    
015200     BLOCK CONTAINS 0.                                                    
015300     SKIP2                                                                
015400 01  IMP-SA                   PIC X(80).                                  
015500     SKIP2                                                                
015600*01  RIO-POST-SA       -COPY W461RIO2   -L.                               
015700     EJECT                                                                
015800 FD  W46176                                                               
015900     RECORDING       V                                                    
016000     BLOCK CONTAINS 0.                                                    
016100     SKIP2                                                                
016200 01  IMP-PE                   PIC X(80).                                  
016300     SKIP2                                                                
016400*01  RIO-POST-PE       -COPY W461RIO2   -L.                               
016500     EJECT                                                                
016600 FD  W46173                                                               
016700     RECORDING       V                                                    
016800     BLOCK CONTAINS 0.                                                    
016900     SKIP2                                                                
017000 01  IMP-FR                   PIC X(80).                                  
017100     SKIP2                                                                
017200*01  RIO-POST-FR       -COPY W461RIO2 -PRE SOFT-  -L.                     
017300     EJECT                                                                
017400 FD  W46170                                                               
017500     RECORDING       V                                                    
017600     BLOCK CONTAINS 0.                                                    
017700     SKIP2                                                                
017800 01  IMP-SE                   PIC X(117).                                 
017900     SKIP2                                                                
018000*01  RIO-POST-SE       -COPY W461RIO2 -PRE SOFT-  -L.                     
018100     EJECT                                                                
018200 FD  W46140                                                               
018300     RECORDING       V                                                    
018400     BLOCK CONTAINS 0.                                                    
018500     SKIP2                                                                
018600 01  IMP-DK                   PIC X(80).                                  
018700     SKIP2                                                                
018800*01  RIO-POST-DK       -COPY W461RIO2 -PRE SOFT-  -L.                     
018900     EJECT                                                                
019000 FD  W46139                                                               
019100     RECORDING       V                                                    
019200     BLOCK CONTAINS 0.                                                    
019300     SKIP2                                                                
019400 01  IMP-NO                   PIC X(80).                                  
019500     SKIP2                                                                
019600*01  RIO-POST-NO       -COPY W461RIO2 -PRE SOFT-  -L.                     
019700     EJECT                                                                
019800 FD  W46174                                                               
019900     RECORDING       V                                                    
020000     BLOCK CONTAINS 0.                                                    
020100     SKIP2                                                                
020200 01  IMP-CH2070               PIC X(80).                                  
020300     SKIP2                                                                
020400*01  RIO-POST-CH2070   -COPY W461RIO2 -PRE SOFT-  -L.                     
020500     EJECT                                                                
020600 FD  W46175                                                               
020700     RECORDING       V                                                    
020800     BLOCK CONTAINS 0.                                                    
020900     SKIP2                                                                
021000 01  IMP-BR                   PIC X(80).                                  
021100     SKIP2                                                                
021200*01  RIO-POST-BR       -COPY W461RIO2   -L.                               
021300     EJECT                                                                
021400 FD  W46131                                                               
021500     RECORDING       V                                                    
021600     BLOCK CONTAINS 0.                                                    
021700     SKIP2                                                                
021800 01  IMP-AU7836               PIC X(80).                                  
021900     SKIP2                                                                
022000*01  RIO-POST-AU7836   -COPY W461RIO2 -PRE SOFT-  -L.                     
022100     EJECT                                                                
022200 FD  W46180                                                               
022300     RECORDING       V                                                    
022400     BLOCK CONTAINS 0.                                                    
022500     SKIP2                                                                
022600 01  IMP-AU7838               PIC X(80).                                  
022700     SKIP2                                                                
022800*01  RIO-POST-AU7838   -COPY W461RIO2 -PRE SOFT-  -L.                     
022900     EJECT                                                                
023000 FD  W4612Z                                                               
023100     RECORDING       V                                                    
023200     BLOCK CONTAINS 0.                                                    
023300     SKIP2                                                                
023400 01  IMP-FI1091              PIC X(80).                                   
023500     SKIP2                                                                
023600*01  RIO-POST-FI1091   -COPY W461RIO2 -PRE SOFT-  -L.                     
023700     EJECT                                                                
023800 FD  W46134                                                               
023900     RECORDING       V                                                    
024000     BLOCK CONTAINS 0.                                                    
024100     SKIP2                                                                
024200 01  IMP-TW                   PIC X(80).                                  
024300     SKIP2                                                                
024400*01  RIO-POST-TW       -COPY W461RIO2 -PRE SOFT-  -L.                     
024500     EJECT                                                                
024600 FD  W4612A                                                               
024700     RECORDING       V                                                    
024800     BLOCK CONTAINS 0.                                                    
024900     SKIP2                                                                
025000 01  IMP-TW2                  PIC X(80).                                  
025100     SKIP2                                                                
025200*01  RIO-POST-TW2      -COPY W461RIO2 -PRE SOFT-  -L.                     
025300     EJECT                                                                
025400 FD  W4612H                                                               
025500     RECORDING       V                                                    
025600     BLOCK CONTAINS 0.                                                    
025700     SKIP2                                                                
025800 01  IMP-JP                   PIC X(80).                                  
025900     SKIP2                                                                
026000*01  RIO-POST-JP       -COPY W461RIO2 -PRE SOFT-  -L.                     
026100     EJECT                                                                
026200 FD  W46128                                                               
026300     RECORDING       V                                                    
026400     BLOCK CONTAINS 0.                                                    
026500     SKIP2                                                                
026600 01  IMP-JP5220               PIC X(80).                                  
026700     SKIP2                                                                
026800*01  RIO-POST-JP5220   -COPY W461RIO2 -PRE SOFT- -L.                      
026900     EJECT                                                                
027000 FD  W4612B                                                               
027100     RECORDING       V                                                    
027200     BLOCK CONTAINS 0.                                                    
027300     SKIP2                                                                
027400 01  IMP-GB1378               PIC X(80).                                  
027500     SKIP2                                                                
027600*01  RIO-POST-GB1378   -COPY W461RIO2 -PRE SOFT-  -L.                     
027700     EJECT                                                                
027800 FD  W4612C                                                               
027900     RECORDING       V                                                    
028000     BLOCK CONTAINS 0.                                                    
028100     SKIP2                                                                
028200 01  IMP-PL                   PIC X(80).                                  
028300     SKIP2                                                                
028400*01  RIO-POST-PL       -COPY W461RIO2 -PRE SOFT-  -L.                     
028500     EJECT                                                                
028600 FD  W4612D                                                               
028700     RECORDING       V                                                    
028800     BLOCK CONTAINS 0.                                                    
028900     SKIP2                                                                
029000 01  IMP-CAN               PIC X(80).                                     
029100     SKIP2                                                                
029200*01  RIO-POST-CAN      -COPY W461RIO2 -PRE SOFT-  -L.                     
029300     EJECT                                                                
029400 FD  W4612E                                                               
029500     RECORDING       V                                                    
029600     BLOCK CONTAINS 0.                                                    
029700     SKIP2                                                                
029800 01  IMP-USA                  PIC X(80).                                  
029900     SKIP2                                                                
030000*01  RIO-POST-USA      -COPY W461RIO2 -PRE SOFT-  -L.                     
030100     EJECT                                                                
030200 FD  W4612F                                                               
030300     RECORDING       V                                                    
030400     BLOCK CONTAINS 0.                                                    
030500     SKIP2                                                                
030600 01  IMP-KR                   PIC X(80).                                  
030700     SKIP2                                                                
030800*01  RIO-POST-KR       -COPY W461RIO2 -PRE SOFT-  -L.                     
030900     EJECT                                                                
031000 FD  W4612G                                                               
031100     RECORDING       V                                                    
031200     BLOCK CONTAINS 0.                                                    
031300     SKIP2                                                                
031400 01  IMP-PT                   PIC X(80).                                  
031500     SKIP2                                                                
031600*01  RIO-POST-PT       -COPY W461RIO2 -PRE SOFT-  -L.                     
031700     SKIP2                                                                
031800     EJECT                                                                
031900 FD  W4612I                                                               
032000     RECORDING       V                                                    
032100     BLOCK CONTAINS 0.                                                    
032200     SKIP2                                                                
032300 01  IMP-MY                   PIC X(80).                                  
032400     SKIP2                                                                
032500*01  RIO-POST-MY       -COPY W461RIO2 -PRE SOFT-  -L.                     
032600     EJECT                                                                
032700 FD  W4612J                                                               
032800     RECORDING       V                                                    
032900     BLOCK CONTAINS 0.                                                    
033000     SKIP2                                                                
033100 01  IMP-TH                   PIC X(80).                                  
033200     SKIP2                                                                
033300*01  RIO-POST-TH       -COPY W461RIO2 -PRE SOFT-  -L.                     
033400     EJECT                                                                
033500 FD  W4612K                                                               
033600     RECORDING       V                                                    
033700     BLOCK CONTAINS 0.                                                    
033800     SKIP2                                                                
033900 01  IMP-IE                   PIC X(80).                                  
034000     SKIP2                                                                
034100*01  RIO-POST-IE       -COPY W461RIO2 -PRE SOFT-  -L.                     
034200     EJECT                                                                
034300 FD  W4612L                                                               
034400     RECORDING       V                                                    
034500     BLOCK CONTAINS 0.                                                    
034600     SKIP2                                                                
034700 01  IMP-BR-NEW               PIC X(80).                                  
034800     SKIP2                                                                
034900*01  RIO-POST-BR-NEW   -COPY W461RIO2 -PRE SOFT-  -L.                     
035000     EJECT                                                                
035100 FD  W4612M                                                               
035200     RECORDING       V                                                    
035300     BLOCK CONTAINS 0.                                                    
035400     SKIP2                                                                
035500 01  IMP-MX                   PIC X(80).                                  
035600     SKIP2                                                                
035700*01  RIO-POST-MX       -COPY W461RIO2 -PRE SOFT-  -L.                     
035800     EJECT                                                                
035900 FD  W4612N                                                               
036000     RECORDING       V                                                    
036100     BLOCK CONTAINS 0.                                                    
036200     SKIP2                                                                
036300 01  IMP-TR                   PIC X(80).                                  
036400     SKIP2                                                                
036500*01  RIO-POST-TR       -COPY W461RIO2 -PRE SOFT-  -L.                     
036600     EJECT                                                                
036700 FD  W461RU                                                               
036800     RECORDING       V                                                    
036900     BLOCK CONTAINS 0.                                                    
037000     SKIP2                                                                
037100 01  IMP-RU                   PIC X(80).                                  
037200     SKIP2                                                                
037300*01  RIO-POST-RU       -COPY W461RIO2 -PRE SOFT-  -L.                     
037400     EJECT                                                                
037500 FD  W461CN                                                               
037600     RECORDING       V                                                    
037700     BLOCK CONTAINS 0.                                                    
037800     SKIP2                                                                
037900 01  IMP-CN                   PIC X(80).                                  
038000     SKIP2                                                                
038100*01  RIO-POST-CN       -COPY W461RIO2 -PRE SOFT-  -L.                     
038200     EJECT                                                                
038300 FD  W461CN1                                                              
038400     RECORDING       V                                                    
038500     BLOCK CONTAINS 0.                                                    
038600     SKIP2                                                                
038700 01  IMP-CN1                  PIC X(80).                                  
038800     SKIP2                                                                
038900*01  RIO-POST-CN1      -COPY W461RIO2 -PRE SOFT-  -L.                     
039000     EJECT                                                                
039100 FD  W461ZA                                                               
039200     RECORDING       V                                                    
039300     BLOCK CONTAINS 0.                                                    
039400     SKIP2                                                                
039500 01  IMP-ZA                   PIC X(80).                                  
039600     SKIP2                                                                
039700*01  RIO-POST-ZA       -COPY W461RIO2 -PRE SOFT-  -L.                     
039800     EJECT                                                                
039900 FD  W461PT2                                                              
040000     RECORDING       V                                                    
040100     BLOCK CONTAINS 0.                                                    
040200     SKIP2                                                                
040300 01  IMP-PT2                  PIC X(80).                                  
040400     SKIP2                                                                
040500*01  RIO-POST-PT2      -COPY W461RIO2 -PRE SOFT-  -L.                     
040600     SKIP2                                                                
040700     EJECT                                                                
040800 FD  W461IN                                                               
040900     RECORDING       V                                                    
041000     BLOCK CONTAINS 0.                                                    
041100     SKIP2                                                                
041200 01  IMP-IN                   PIC X(80).                                  
041300     SKIP2                                                                
041400*01  RIO-POST-IN       -COPY W461RIO2 -PRE SOFT-  -L.                     
041500     EJECT                                                                
041600 FD  W461CZ                                                               
041700     RECORDING       V                                                    
041800     BLOCK CONTAINS 0.                                                    
041900     SKIP2                                                                
042000 01  IMP-CZ                   PIC X(80).                                  
042100     SKIP2                                                                
042200*01  RIO-POST-CZ       -COPY W461RIO2 -PRE SOFT-  -L.                     
042300     EJECT                                                                
042400 FD  W461HU                                                               
042500     RECORDING       V                                                    
042600     BLOCK CONTAINS 0.                                                    
042700     SKIP2                                                                
042800 01  IMP-HU                   PIC X(80).                                  
042900     SKIP2                                                                
043000*01  RIO-POST-HU       -COPY W461RIO2 -PRE SOFT-  -L.                     
043100     EJECT                                                                
043200 WORKING-STORAGE SECTION.                                                 
043300                                                                          
043400*    -- CHECKED BY WY2000                                                 
043500*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
043600 77  IDPGM                       PIC X(8)    VALUE 'W4761800'.            
043700     SKIP2                                                                
043800*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
043900                                                                          
044000 77  JA                          PIC X(1)    VALUE 'J'.                   
044100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
044200 77  YES                         PIC X(1)    VALUE 'Y'.                   
044300                                                                          
044400*- - - - - - - - - - - - - -  TVÅ-STÄLLIGA ISO-KODER                      
044500*01  -COPY W460LISO                                                       
044600     EJECT                                                                
044700*- - - - - - - - - - - - - -  SWITCHAR                                    
044800 77  FL-GODKEND-KDFAKTYP         PIC X(1).                                
044900 77  LAGRAD-KDLIDEL              PIC S9(1)   VALUE +9.                    
045000 77  W-IDLOPNRE                  PIC S9(3).                               
045100     SKIP2                                                                
045200*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
045300                                                                          
045400 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
045500     SKIP2                                                                
045600 01  W-VKORDBTO-ORDER-LB         PIC S9(8)V9(1) COMP-3 VALUE ZERO.        
045700 01  SPAR-IDPTYP                 PIC X(3)  VALUE SPACE.                   
045800 01  SPAR-IDORDNR                PIC S9(7) COMP-3.                        
045900 01  SPAR-BEVOLREF               PIC X(10).                               
046000 01  SPAR-IDDC                   PIC X(2).                                
046100 01  SPAR-IDDISTR                PIC S9(5) VALUE ZERO  COMP-3.            
046200*      --- VALID IDDC CODES                                               
046300*                                                                         
046400                                                                          
046500 01  WS-IDDC                     PIC X(2)  VALUE SPACE.                   
046600 01  WS-KDVALUTA                 PIC 9(3)  VALUE ZERO.                    
046700 01  SPAR-RIM-IDORDNR            PIC 9(7)  VALUE ZERO.                    
046800 01  SPAR-RIM-IDKUNDNR           PIC 9(6)  VALUE ZERO.                    
046900     SKIP2                                                                
047000 01  SPAR-RIL-IDPTYPA            PIC X(3)  VALUE SPACE.                   
047100 01  -COPY W461RILN -PRE SPAR-                                            
047200                                                                          
047300 01  SPAR-IDTRPTNR               PIC 9(3)       VALUE ZERO.               
047400 01  SPAR-IDLBBET                PIC X(12)      VALUE SPACE.              
047500                                                                          
047600 01  WS-TIAAMMDD                 PIC 9(6).                                
047700 01  WS-DELAD-TIAAMMDD  REDEFINES WS-TIAAMMDD.                            
047800     03  FILLER                  PIC 9(2).                                
047900     03  WS-TIMM                 PIC 9(2).                                
048000     03  WS-TIDD                 PIC 9(2).                                
048100     EJECT                                                                
048200 01  TEST-IDDISTR                PIC 9(5) COMP-3.                         
048300*01  FILLER -COPY WWDIS100 -RED TEST-IDDISTR.                             
048400     SKIP3                                                                
048500*01  FILLER -COPY WWDIS130 -RED TEST-IDDISTR.                             
048600     SKIP3                                                                
048700*01  FILLER -COPY WWDIS102 -RED TEST-IDDISTR.                             
048800     SKIP3                                                                
048900*01  FILLER -COPY WWDIST35 -RED TEST-IDDISTR.                             
049000     SKIP3                                                                
049100*01  FILLER -COPY WWDIS121 -RED TEST-IDDISTR.                             
049200     SKIP3                                                                
049300 01  DYNAMISKA-SUBPROGRAM.                                                
049400   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
049500   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
049600   03  W460DIS1                  PIC X(8)    VALUE 'W460DIS1'.            
049700     SKIP3                                                                
049800*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
049900                                                                          
050000 01  RETURKODER.                                                          
050100   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
050200   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
050300   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
050400     EJECT                                                                
050500*- - - - - - - - - - - - - -  INDEX ETC.                                  
050600     SKIP2                                                                
050700 01  IX-STATNR             PIC S9(3) COMP-3.                              
050800 01  IX-RAD                PIC S9(5) COMP-3.                              
050900 01  IX-KOLUMN             PIC S9(5) COMP-3.                              
051000 01  KDHBLKRV              PIC S9(5) COMP-3.                              
051100     EJECT                                                                
051200*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
051300                                                                          
051400*01  -COPY W0005       -PRE  POSTSUM-.                                    
051500     EJECT                                                                
051600*- - - - - - - - - - - - - -  PARAMETRAR TILL W460DIS1                    
051700                                                                          
051800*01  -COPY W460DIS1                                                       
051900     EJECT                                                                
052000*    --- PARAMETRAR TILL COPYTEXT   WWOMVAND                              
052100*01 -COPY WWOMVAND                                                        
052200     EJECT                                                                
052300*    --- PARAMETRAR TILL W930VAL                                          
052400*01 -COPY W930VAL                                                         
052500     EJECT                                                                
052600******************************************************************        
052700*         SORTERADE POSTER                                       *        
052800******************************************************************        
052900 01  WSORT-AREA.                                                          
053000   03  WSORT-AREA-X          PIC X(211).                                  
053100     SKIP3                                                                
053200*  03  FILLER -COPY W461RIO2   -PRE WSORT- -RED WSORT-AREA-X.             
053300     EJECT                                                                
053400******************************************************************        
053500*    POSTER TILL IMPORTÖR, KORTFORMAT.                           *        
053600******************************************************************        
053700     SKIP2                                                                
053800 01  UTKORT-RIM2.                                                         
053900*    03  -COPY W461RIM2                                                   
054000     EJECT                                                                
054100 01  UTKORT.                                                              
054200   03  UTKORT-X             PIC X(80).                                    
054300     SKIP3                                                                
054400*  03 FILLER -COPY W461RIJN   -RED UTKORT-X.                              
054500     EJECT                                                                
054600*  03 FILLER -COPY W461RIK1   -RED UTKORT-X.                              
054700     EJECT                                                                
054800*  03 FILLER -COPY W461RILN   -RED UTKORT-X.                              
054900     EJECT                                                                
055000*  03 FILLER -COPY W461RINN   -RED UTKORT-X.                              
055100     EJECT                                                                
055200*  03 FILLER -COPY W461RIPN   -RED UTKORT-X.                              
055300     EJECT                                                                
055400*  03 FILLER -COPY W461RIZN   -RED UTKORT-X.                              
055500     EJECT                                                                
055600*                                                                         
055700 01  UTKORT-LONG.                                                         
055800     SKIP3                                                                
055900   03  UTKORT-LONGX    -COPY W461RIO2 -L.                                 
056000     SKIP3                                                                
056100*  03 FILLER -COPY W461RIO1   -RED UTKORT-LONGX.                          
056200     EJECT                                                                
056300*  03 FILLER -COPY W461RIO2   -PRE SOFT- -RED UTKORT-LONGX.               
056400     EJECT                                                                
056500 PROCEDURE DIVISION.                                                      
056600     SKIP2                                                                
056700     PERFORM A-INIT                                                       
056800                                                                          
056900     PERFORM B-BEARBETNING                                                
057000     SKIP2                                                                
057100     PERFORM Z-FINIT                                                      
057200     MOVE ZERO TO RETURN-CODE                                             
057300     GOBACK                                                               
057400                                                                          
057500     .                                                                    
057600     EJECT                                                                
057700 A-INIT SECTION.                                                          
057800     OPEN INPUT INFIL                                                     
057900     OPEN OUTPUT                                                          
058000     W46129                                                               
058100     W46127                                                               
058200     W46130                                                               
058300     W4612Z                                                               
058400     W46138                                                               
058500     W46179                                                               
058600     W46168                                                               
058700     W46171                                                               
058800     W46161                                                               
058900     W46176                                                               
059000     W46178                                                               
059100     W46139                                                               
059200     W46170                                                               
059300     W46140                                                               
059400     W46173                                                               
059500     W46131                                                               
059600     W46174                                                               
059700     W46172                                                               
059800     W46175                                                               
059900     W46180 W46134                                                        
060000     W4612A W4612H                                                        
060100     W4612B                                                               
060200     W46128                                                               
060300     W4612C                                                               
060400     W4612D W4612E                                                        
060500     W4612F                                                               
060600     W4612G                                                               
060700     W4612K                                                               
060800     W4612I W4612J                                                        
060900     W4612L W4612M                                                        
061000     W4612N                                                               
061100     W461RU                                                               
061200     W461CN                                                               
061300     W461CN1                                                              
061400     W461ZA                                                               
061500     W461PT2                                                              
061600     W461IN                                                               
061700     W461CZ W461HU                                                        
061800     MOVE SPACE TO UTKORT UTKORT-RIM2                                     
061900     .                                                                    
062000     EJECT                                                                
062100 B-BEARBETNING SECTION.                                                   
062200     SKIP2                                                                
062300     PERFORM S01-LAS-SORTERAD-INFIL                                       
062400     PERFORM UNTIL SORTFIL-EOF = JA                                       
062500         EVALUATE TRUE                                                    
062600         WHEN WSORT-RIO-IDPTYP = 'RIK'                                    
062700           PERFORM BJ-FAKTURA-HUVUD-1                                     
062800         WHEN WSORT-RIO-IDPTYP = 'RIL'                                    
062900           PERFORM BK-FAKTURA-HUVUD-2                                     
063000         WHEN WSORT-RIO-IDPTYP = 'RIM'                                    
063100           PERFORM BL-FAKTURA-REFERENS                                    
063200         WHEN WSORT-RIO-IDPTYP = 'RIN'                                    
063300           PERFORM BM-FAKTURA-KOLLI                                       
063400         WHEN WSORT-RIO-IDPTYP = 'RIO'                                    
063500           PERFORM BN-FAKTURA-RAD-1                                       
063600         WHEN WSORT-RIO-IDPTYP = 'RIP'                                    
063700           PERFORM BP-FAKTURA-RAD-2                                       
063800         WHEN WSORT-RIO-IDPTYP = 'RIZ'                                    
063900           PERFORM BO-FAKTURA-RAD-Z                                       
064000         END-EVALUATE                                                     
064100       MOVE WSORT-RIO-IDPTYP TO SPAR-IDPTYP                               
064200       PERFORM S01-LAS-SORTERAD-INFIL                                     
064300     END-PERFORM                                                          
064400     .                                                                    
064500 BJ-FAKTURA-HUVUD-1 SECTION.                                              
064600     SKIP2                                                                
064700     MOVE NEJ TO FL-GODKEND-KDFAKTYP                                      
064800     MOVE WSORT-AREA              TO RIK-W461RIK1                         
064900     IF RIK-KDFAKTYP = 'R' OR 'G' OR 'K'                                  
065000       MOVE JA TO FL-GODKEND-KDFAKTYP                                     
065100       MOVE RIK-IDDC              TO SPAR-IDDC                            
065200       MOVE RIK-IDDISTR           TO SPAR-IDDISTR                         
065300                                                                          
065400       PERFORM S10-SPLITTA-IMPORTORER                                     
065500     END-IF                                                               
065600     MOVE SPACE TO UTKORT UTKORT-RIM2                                     
065700     .                                                                    
065800     EJECT                                                                
065900                                                                          
066000 BK-FAKTURA-HUVUD-2 SECTION.                                              
066100     SKIP2                                                                
066200     IF FL-GODKEND-KDFAKTYP = JA                                          
066300                                                                          
066400       MOVE SPAR-IDDISTR           TO TEST-IDDISTR                        
066500       IF DIS102-ITALIEN                                                  
066600         MOVE 'RIL'                TO SPAR-RIL-IDPTYPA                    
066700         MOVE WSORT-AREA           TO SPAR-RIL-W461RILN-CTX               
066800       ELSE                                                               
066900         MOVE WSORT-AREA           TO RIL-W461RILN-CTX                    
067000         PERFORM S10-SPLITTA-IMPORTORER                                   
067100       END-IF                                                             
067200     END-IF                                                               
067300     MOVE SPACE TO UTKORT UTKORT-RIM2                                     
067400     .                                                                    
067500     EJECT                                                                
067600 BL-FAKTURA-REFERENS SECTION.                                             
067700** DET SKAPAS TVÅ POSTTYP '011' PÅ ORDER SOM HAR BLANDAT                  
067800** VANLIGA ARTIKLAR OCH DIREKLEVERANSARTIKLAR. DET SKA                    
067900** BARA SKAPAS EN 'RIM'-POST TILL VIPS                                    
068000     SKIP2                                                                
068100     IF SPAR-RIL-IDPTYPA = 'RIL'                                          
068200         MOVE 'RIL'                 TO RIL-IDPTYP                         
068300         MOVE SPAR-RIL-W461RILN-CTX TO RIL-W461RILN-CTX                   
068400         PERFORM S10-SPLITTA-IMPORTORER                                   
068500         MOVE SPACE TO SPAR-RIL-IDPTYPA                                   
068600         MOVE SPACE TO UTKORT UTKORT-RIM2                                 
068700     END-IF                                                               
068800                                                                          
068900     IF FL-GODKEND-KDFAKTYP = JA                                          
069000        MOVE WSORT-AREA               TO RIM-W461RIM2-CTX                 
069100        IF RIM-IDKUNDNR    = SPAR-RIM-IDKUNDNR                            
069200           AND RIM-IDORDNR = SPAR-RIM-IDORDNR                             
069300           AND SPAR-IDPTYP        = 'RIM'                                 
069400           MOVE RIM-IDKUNDNR          TO SPAR-RIM-IDKUNDNR                
069500           MOVE RIM-IDORDNR           TO SPAR-RIM-IDORDNR                 
069600           CONTINUE                                                       
069700        ELSE                                                              
069800           MOVE SPAR-IDDC             TO WS-IDDC                          
069900           PERFORM S10-SPLITTA-IMPORTORER                                 
070000           MOVE RIM-IDKUNDNR          TO SPAR-RIM-IDKUNDNR                
070100           MOVE RIM-IDORDNR           TO SPAR-RIM-IDORDNR                 
070200        END-IF                                                            
070300     END-IF                                                               
070400     MOVE SPACE TO UTKORT UTKORT-RIM2                                     
070500     .                                                                    
070600     EJECT                                                                
070700 BM-FAKTURA-KOLLI SECTION.                                                
070800     SKIP2                                                                
070900     IF FL-GODKEND-KDFAKTYP = JA                                          
071000       MOVE WSORT-AREA              TO RIN-W461RINN-CTX                   
071010                                                                          
071100       PERFORM S10-SPLITTA-IMPORTORER                                     
071200     END-IF                                                               
071300     MOVE SPACE TO UTKORT UTKORT-RIM2                                     
071400     .                                                                    
071500     EJECT                                                                
071600 BN-FAKTURA-RAD-1 SECTION.                                                
071700     SKIP2                                                                
071800     IF FL-GODKEND-KDFAKTYP = JA                                          
071900                                                                          
072000       MOVE SPAR-IDDISTR     TO DIS1-IDDISTR                              
072100                                TEST-IDDISTR                              
072200       CALL W460DIS1 USING DIS1-W460DIS1                                  
072300       IF DIS1-IDLANDX2 = ISO-SAUDI     OR                                
072400          DIS1-IDLANDX2 = ISO-PERU      OR                                
072500         (DIS1-IDLANDX2 = ISO-BRASILIEN AND DIS121-BRASIL)                
072600         MOVE WSORT-AREA          TO RIO-W461RIO1-CTX                     
072700         MOVE SPAR-IDDC           TO RIO-IDDC                             
072800         PERFORM S10-SPLITTA-IMPORTORER                                   
072900         MOVE SPACE TO UTKORT-LONG                                        
073000       ELSE                                                               
073100         MOVE WSORT-AREA          TO SOFT-RIO-W461RIO2                    
073200* SVERIGE VIPS KLARAR INTE ALPHA DC - TAS FRÅN HUVUD ISTÄLLET             
073300*        MOVE WSORT-FRAD-IDDC     TO SOFT-RIO-IDDC                        
073400         MOVE SPAR-IDDC           TO SOFT-RIO-IDDC                        
073500         PERFORM S10-SPLITTA-IMPORTORER                                   
073600         MOVE SPACE TO UTKORT-LONG                                        
073700       END-IF                                                             
073800     END-IF                                                               
073900                                                                          
074000     .                                                                    
074100     EJECT                                                                
074200                                                                          
074300 BP-FAKTURA-RAD-2 SECTION.                                                
074400     SKIP2                                                                
074500     IF FL-GODKEND-KDFAKTYP = JA                                          
074600       MOVE WSORT-AREA             TO RIP-W461RIPN-CTX                    
074700       PERFORM S10-SPLITTA-IMPORTORER                                     
074800       MOVE SPACE TO UTKORT UTKORT-RIM2                                   
074900     END-IF                                                               
075000     .                                                                    
075100     EJECT                                                                
075200 BO-FAKTURA-RAD-Z SECTION.                                                
075300     SKIP2                                                                
075400     IF FL-GODKEND-KDFAKTYP = JA                                          
075500       MOVE WSORT-AREA             TO RIZ-W461RIZN-CTX                    
075600       PERFORM S10-SPLITTA-IMPORTORER                                     
075700       MOVE SPACE TO UTKORT UTKORT-RIM2                                   
075800     END-IF                                                               
075900     .                                                                    
076000     EJECT                                                                
076100 S01-LAS-SORTERAD-INFIL SECTION.                                          
076200     SKIP3                                                                
076300     READ   INFIL   INTO WSORT-AREA                                       
076400                      AT END MOVE JA TO SORTFIL-EOF                       
076500     END-READ                                                             
076600                                                                          
076700     IF SORTFIL-EOF = NEJ                                                 
076800                                                                          
076900       MOVE 'INFIL'             TO POSTSUM-FDNAMN                         
077000       MOVE 'W47618D1'          TO POSTSUM-DDNAMN2                        
077100       MOVE RIO-IDPTYP          TO POSTSUM-TRANSTYP                       
077200       CALL POSTSUM   USING POSTSUM-PARM                                  
077300                                                                          
077400     END-IF                                                               
077500     .                                                                    
077600     EJECT                                                                
077700******************************************************************        
077800*                                                                         
077900*    I S10-SPLITTA-IMPORTORER SECTION VÄXLAS DE OLIKA                     
078000*    IMPORTÖRS-POSTERNA IN PÅ RÄTT FIL.                                   
078100*       BYGG PÅ IF-SATSEN MED NYA DISTRIKT OCH LÄGG                       
078200*    TILL YTTERLIGA S10-SKRIV-W461XX SECTIONER.                           
078300*                                                                         
078400******************************************************************        
078500 S10-SPLITTA-IMPORTORER SECTION.                                          
078600     SKIP2                                                                
078700     MOVE SPAR-IDDISTR   TO TEST-IDDISTR DIS1-IDDISTR                     
078800                                                                          
078900     CALL W460DIS1 USING DIS1-W460DIS1                                    
079000                                                                          
079100     IF (DIS1-IDLANDX2 = ISO-ITALIEN)                                     
079200       OR SPAR-IDDISTR = ZERO                                             
079300       PERFORM S10-SKRIV-W46127                                           
079400     END-IF                                                               
079500     IF (DIS1-IDLANDX2 = ISO-FINLAND)                                     
079600       OR DIS100-FINLLEVANM                                               
079700       OR SPAR-IDDISTR = ZERO                                             
079800       PERFORM S10-SKRIV-W46129                                           
079900     END-IF                                                               
080000     IF (DIS1-IDLANDX2 = ISO-BELGIEN)                                     
080100       OR SPAR-IDDISTR = ZERO                                             
080200       PERFORM S10-SKRIV-W46130                                           
080300     END-IF                                                               
080400     IF (DIS1-IDLANDX2 = ISO-ENGLAND)                                     
080500       OR SPAR-IDDISTR = ZERO                                             
080600       PERFORM S10-SKRIV-W4612B                                           
080700     END-IF                                                               
080800     IF (DIS1-IDLANDX2 = ISO-IRLAND)                                      
080900       OR SPAR-IDDISTR = ZERO                                             
081000       PERFORM S10-SKRIV-W4612K                                           
081100     END-IF                                                               
081200     IF ((DIS1-IDLANDX2 = ISO-USA)                                        
081300        AND (DIS102-USA))                                                 
081400       OR SPAR-IDDISTR = ZERO                                             
081500       PERFORM S10-SKRIV-W46138                                           
081600     END-IF                                                               
081700     IF (DIS1-IDLANDX2 = ISO-TYSKLAND)                                    
081800       OR SPAR-IDDISTR = ZERO                                             
081900       PERFORM S10-SKRIV-W46171                                           
082000     END-IF                                                               
082100     IF (DIS1-IDLANDX2 = ISO-HOLLAND)                                     
082200       OR SPAR-IDDISTR = ZERO                                             
082300       PERFORM S10-SKRIV-W46161                                           
082400     END-IF                                                               
082500     IF (DIS1-IDLANDX2 = ISO-SPANIEN)                                     
082600       OR DIS100-SPANLEVANM-PV                                            
082700       OR DIS130-NOAC-SPANIEN                                             
082800       OR SPAR-IDDISTR = ZERO                                             
082900       PERFORM S10-SKRIV-W46179                                           
083000     END-IF                                                               
083100     IF (DIS1-IDLANDX2 = ISO-OSTERRIKE)                                   
083200       OR SPAR-IDDISTR = ZERO                                             
083300       PERFORM S10-SKRIV-W46168                                           
083400     END-IF                                                               
083500     IF (DIS1-IDLANDX2 = ISO-SAUDI)                                       
083600       OR SPAR-IDDISTR = ZERO                                             
083700       PERFORM S10-SKRIV-W46178                                           
083800     END-IF                                                               
083900     IF (DIS1-IDLANDX2 = ISO-PERU)                                        
084000       OR SPAR-IDDISTR = ZERO                                             
084100       PERFORM S10-SKRIV-W46176                                           
084200     END-IF                                                               
084300     IF (DIS1-IDLANDX2 = ISO-FRANKRIKE)                                   
084400       OR DIS100-FRANLEVANM-PV                                            
084500       OR SPAR-IDDISTR = ZERO                                             
084600       PERFORM S10-SKRIV-W46173                                           
084700     END-IF                                                               
084800     IF (DIS1-IDLANDX2 = ISO-SVERIGE)                                     
084900       OR SPAR-IDDISTR = ZERO                                             
085000       PERFORM S10-SKRIV-W46170                                           
085100     END-IF                                                               
085200     IF (DIS1-IDLANDX2 = ISO-DANMARK)                                     
085300       OR DIS100-DANMLEVANM-PV                                            
085400       OR SPAR-IDDISTR = ZERO                                             
085500       PERFORM S10-SKRIV-W46140                                           
085600     END-IF                                                               
085700     IF (DIS1-IDLANDX2 = ISO-NORGE)                                       
085800       OR DIS100-NORGLEVANM-PV                                            
085900       OR DIST35-CDC-3J-REFILL                                            
086000       OR SPAR-IDDISTR = ZERO                                             
086100       PERFORM S10-SKRIV-W46139                                           
086200     END-IF                                                               
086300     IF DIS130-NOAC-SCHWEIZ-PV                                            
086400       OR SPAR-IDDISTR = ZERO                                             
086500       PERFORM S10-SKRIV-W46174                                           
086600     END-IF                                                               
086700     IF (DIS1-IDLANDX2 = ISO-BRASILIEN AND DIS121-BRASIL)                 
086800       OR SPAR-IDDISTR = ZERO                                             
086900       PERFORM S10-SKRIV-W46175                                           
087000     END-IF                                                               
087100     IF (DIS1-IDLANDX2 = ISO-BRASILIEN AND DIS121-BRASIL-NEW)             
087200       OR DIST35-CDC-BR-REFILL                                            
087201       OR SPAR-IDDISTR = ZERO                                             
087300       PERFORM S10-SKRIV-W4612L                                           
087400     END-IF                                                               
087500     IF (DIS1-IDLANDX2 = ISO-MEXICO)                                      
087600       OR DIS130-NOAC-MEXICO                                              
087700       OR SPAR-IDDISTR = ZERO                                             
087800       PERFORM S10-SKRIV-W4612M                                           
087900     END-IF                                                               
088000     IF (DIS1-IDLANDX2 = ISO-TURKIET)                                     
088100       OR DIS130-NOAC-TURKIET                                             
088200       OR SPAR-IDDISTR = ZERO                                             
088300       PERFORM S10-SKRIV-W4612N                                           
088400     END-IF                                                               
088500     IF DIS130-NOAC-AUSTRALIEN                                            
088600*                 DISTR 7836                                              
088700       OR SPAR-IDDISTR = ZERO                                             
088800       PERFORM S10-SKRIV-W46131                                           
088900     END-IF                                                               
089000     IF (DIS1-IDLANDX2 = ISO-SCHWEIZ)                                     
089100       OR DIST35-CDC-3H-REFILL                                            
089200       OR SPAR-IDDISTR = ZERO                                             
089300       PERFORM S10-SKRIV-W46172                                           
089400     END-IF                                                               
089500     IF (DIS1-IDLANDX2 = ISO-AUSTRALIEN)                                  
089600*                 DISTR 7838                                              
089700       OR SPAR-IDDISTR = ZERO                                             
089800       PERFORM S10-SKRIV-W46180                                           
089900     END-IF                                                               
090000     IF (DIS1-IDLANDX2 = ISO-TAIWAN)                                      
090100       OR DIS130-NOAC-TAIWAN                                              
090200       OR SPAR-IDDISTR = ZERO                                             
090300       PERFORM S10-SKRIV-W46134                                           
090400     END-IF                                                               
090500     IF (DIS1-IDLANDX2 = ISO-TAIWAN2)                                     
090600       OR DIS130-NOAC-TAIWAN2                                             
090700       OR SPAR-IDDISTR = ZERO                                             
090800       PERFORM S10-SKRIV-W4612A                                           
090900     END-IF                                                               
091000     IF DIS1-IDLANDX2 = ISO-JAPAN                                         
091100       OR SPAR-IDDISTR = ZERO                                             
091200       PERFORM S10-SKRIV-W4612H                                           
091300     END-IF                                                               
091400     IF (DIS1-IDLANDX2 = ISO-THAILAND)                                    
091500       OR DIS130-NOAC-THAILAND                                            
091600       OR SPAR-IDDISTR = ZERO                                             
091700       PERFORM S10-SKRIV-W4612J                                           
091800     END-IF                                                               
091900     IF (DIS1-IDLANDX2 = ISO-MALAYSIA)                                    
092000       OR DIS130-NOAC-MALAYSIA                                            
092100       OR SPAR-IDDISTR = ZERO                                             
092200       PERFORM S10-SKRIV-W4612I                                           
092300     END-IF                                                               
092400     IF DIS100-JAPANLEVANM                                                
092500       OR SPAR-IDDISTR = ZERO                                             
092600       PERFORM S10-SKRIV-W46128                                           
092700     END-IF                                                               
092800     IF (DIS1-IDLANDX2 = ISO-POLEN)                                       
092900       OR DIS130-NOAC-POLEN                                               
093000       OR SPAR-IDDISTR = ZERO                                             
093100       PERFORM S10-SKRIV-W4612C                                           
093200     END-IF                                                               
093300     IF DIS130-NOAC-FINLAND                                               
093400       OR SPAR-IDDISTR = ZERO                                             
093500       PERFORM S10-SKRIV-W4612Z                                           
093600     END-IF                                                               
093700     IF ((DIS1-IDLANDX2 = ISO-USA)                                        
093800       AND  (DIS102-USA-NEW))                                             
093900       OR SPAR-IDDISTR = ZERO                                             
094000       PERFORM S10-SKRIV-W4612E                                           
094100     END-IF                                                               
094200     IF (DIS1-IDLANDX2 = ISO-CANADA)                                      
094300       OR SPAR-IDDISTR = ZERO                                             
094400       PERFORM S10-SKRIV-W4612D                                           
094500     END-IF                                                               
094600     IF (DIS1-IDLANDX2 = ISO-KOREA)                                       
094700       OR SPAR-IDDISTR = ZERO                                             
094800       PERFORM S10-SKRIV-W4612F                                           
094900     END-IF                                                               
095000     IF (DIS1-IDLANDX2 = ISO-PORTUGAL)                                    
095100       OR SPAR-IDDISTR = ZERO                                             
095200       PERFORM S10-SKRIV-W4612G                                           
095300     END-IF                                                               
095400     IF (DIS1-IDLANDX2 = ISO-RYSSLAND)                                    
095500       OR DIS130-NOAC-RYSSLAND                                            
095600       OR SPAR-IDDISTR = ZERO                                             
095700       PERFORM S10-SKRIV-W461RU                                           
095800     END-IF                                                               
095900     IF (DIS1-IDLANDX2 = ISO-KINA)                                        
096000       OR DIS130-NOAC-KINA                                                
096100       OR SPAR-IDDISTR = ZERO                                             
096200       PERFORM S10-SKRIV-W461CN                                           
096300     END-IF                                                               
096400     IF (DIS1-IDLANDX2 = ISO-KINA-C1)                                     
096500       OR DIS130-NOAC-KINA-C1                                             
096600       OR SPAR-IDDISTR = ZERO                                             
096700       PERFORM S10-SKRIV-W461CN1                                          
096800     END-IF                                                               
096900     IF (DIS1-IDLANDX2 = ISO-SYDAFRIKA)                                   
097000       OR DIS130-NOAC-SYDAFRIKA                                           
097100       OR SPAR-IDDISTR = ZERO                                             
097200       PERFORM S10-SKRIV-W461ZA                                           
097300     END-IF                                                               
097400     IF (DIS1-IDLANDX2 = ISO-PORTUGAL2)                                   
097500       OR DIS130-NOAC-PORTUGAL                                            
097600       OR SPAR-IDDISTR = ZERO                                             
097700       PERFORM S10-SKRIV-W461PT2                                          
097800     END-IF                                                               
097900     IF (DIS1-IDLANDX2 = ISO-INDIEN)                                      
098000       OR SPAR-IDDISTR = ZERO                                             
098100       PERFORM S10-SKRIV-W461IN                                           
098200     END-IF                                                               
098300     IF (DIS1-IDLANDX2 = ISO-TJECKIEN)                                    
098400       OR SPAR-IDDISTR = ZERO                                             
098500       PERFORM S10-SKRIV-W461CZ                                           
098600     END-IF                                                               
098700     IF (DIS1-IDLANDX2 = ISO-UNGERN)                                      
098800       OR SPAR-IDDISTR = ZERO                                             
098900       PERFORM S10-SKRIV-W461HU                                           
099000     END-IF                                                               
099100     .                                                                    
099200     EJECT                                                                
099300 S10-SKRIV-W46127 SECTION.                                                
099400*    ITALIEN                                                              
099500     EVALUATE RIO-IDPTYP                                                  
099600       WHEN 'RIO'                                                         
099700         WRITE SOFT-RIO-POST-IT FROM UTKORT-LONG                          
099800         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
099900       WHEN OTHER                                                         
100000         IF RIM-IDPTYP = 'RIM'                                            
100100           WRITE IMP-IT FROM UTKORT-RIM2                                  
100200           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
100300         ELSE                                                             
100400           WRITE IMP-IT FROM UTKORT                                       
100500           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
100600         END-IF                                                           
100700     END-EVALUATE                                                         
100800     MOVE 'W46127'               TO POSTSUM-FDNAMN                        
100900     MOVE 'W47618D3'             TO POSTSUM-DDNAMN2                       
101000     CALL POSTSUM      USING POSTSUM-PARM                                 
101100     .                                                                    
101200     EJECT                                                                
101300 S10-SKRIV-W46129 SECTION.                                                
101400*    FINLAND                                                              
101500     EVALUATE RIO-IDPTYP                                                  
101600       WHEN 'RIO'                                                         
101700         WRITE SOFT-RIO-POST-FI FROM UTKORT-LONG                          
101800         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
101900       WHEN OTHER                                                         
102000         IF WSORT-RIO-IDPTYP = 'RIM'                                      
102100           WRITE IMP-FI FROM UTKORT-RIM2                                  
102200           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
102300         ELSE                                                             
102400           WRITE IMP-FI FROM UTKORT                                       
102500           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
102600         END-IF                                                           
102700     END-EVALUATE                                                         
102800     MOVE 'W46129'               TO POSTSUM-FDNAMN                        
102900     MOVE 'W47618D4'             TO POSTSUM-DDNAMN2                       
103000     CALL POSTSUM      USING POSTSUM-PARM                                 
103100     .                                                                    
103200     EJECT                                                                
103300 S10-SKRIV-W46130 SECTION.                                                
103400*    BELGIEN                                                              
103500     EVALUATE RIO-IDPTYP                                                  
103600       WHEN 'RIO'                                                         
103700         WRITE SOFT-RIO-POST-BE FROM UTKORT-LONG                          
103800         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
103900       WHEN OTHER                                                         
104000         IF WSORT-RIO-IDPTYP = 'RIM'                                      
104100           WRITE IMP-BE FROM UTKORT-RIM2                                  
104200           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
104300         ELSE                                                             
104400           WRITE IMP-BE FROM UTKORT                                       
104500           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
104600         END-IF                                                           
104700     END-EVALUATE                                                         
104800     MOVE 'W46130'               TO POSTSUM-FDNAMN                        
104900     MOVE 'W47618D5'             TO POSTSUM-DDNAMN2                       
105000     CALL POSTSUM      USING POSTSUM-PARM                                 
105100     .                                                                    
105200     EJECT                                                                
105300 S10-SKRIV-W46138 SECTION.                                                
105400*    USA                                                                  
105500     EVALUATE RIO-IDPTYP                                                  
105600       WHEN 'RIO'                                                         
105700         PERFORM S30-JA                                                   
105800         WRITE SOFT-RIO-POST-US FROM UTKORT-LONG                          
105900         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
106000       WHEN OTHER                                                         
106100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
106200           WRITE IMP-US FROM UTKORT-RIM2                                  
106300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
106400         ELSE                                                             
106500           WRITE IMP-US FROM UTKORT                                       
106600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
106700         END-IF                                                           
106800     END-EVALUATE                                                         
106900     MOVE 'W46138'               TO POSTSUM-FDNAMN                        
107000     MOVE 'W47618D8'             TO POSTSUM-DDNAMN2                       
107100     CALL POSTSUM      USING POSTSUM-PARM                                 
107200     .                                                                    
107300     EJECT                                                                
107400 S10-SKRIV-W46171 SECTION.                                                
107500*    TYSKLAND                                                             
107600     EVALUATE RIO-IDPTYP                                                  
107700       WHEN 'RIO'                                                         
107800         WRITE SOFT-RIO-POST-DE FROM UTKORT-LONG                          
107900         MOVE SPAR-IDPTYP       TO POSTSUM-TRANSTYP                       
108000       WHEN OTHER                                                         
108100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
108200           WRITE IMP-DE FROM UTKORT-RIM2                                  
108300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
108400         ELSE                                                             
108500           WRITE IMP-DE FROM UTKORT                                       
108600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
108700         END-IF                                                           
108800     END-EVALUATE                                                         
108900     MOVE 'W46171'               TO POSTSUM-FDNAMN                        
109000     MOVE 'W47618D9'             TO POSTSUM-DDNAMN2                       
109100     CALL POSTSUM      USING POSTSUM-PARM                                 
109200     .                                                                    
109300     EJECT                                                                
109400 S10-SKRIV-W46161 SECTION.                                                
109500*    HOLLAND                                                              
109600     EVALUATE RIO-IDPTYP                                                  
109700       WHEN 'RIO'                                                         
109800         WRITE SOFT-RIO-POST-NL FROM UTKORT-LONG                          
109900         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
110000       WHEN OTHER                                                         
110100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
110200           WRITE IMP-NL FROM UTKORT-RIM2                                  
110300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
110400         ELSE                                                             
110500           WRITE IMP-NL FROM UTKORT                                       
110600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
110700         END-IF                                                           
110800     END-EVALUATE                                                         
110900     MOVE 'W46161'               TO POSTSUM-FDNAMN                        
111000     MOVE 'W47618DA'             TO POSTSUM-DDNAMN2                       
111100     CALL POSTSUM      USING POSTSUM-PARM                                 
111200     .                                                                    
111300     EJECT                                                                
111400 S10-SKRIV-W46179 SECTION.                                                
111500*    SPANIEN                                                              
111600     EVALUATE RIO-IDPTYP                                                  
111700       WHEN 'RIO'                                                         
111800         WRITE SOFT-RIO-POST-ES FROM UTKORT-LONG                          
111900         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
112000       WHEN OTHER                                                         
112100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
112200           WRITE IMP-ES FROM UTKORT-RIM2                                  
112300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
112400         ELSE                                                             
112500           WRITE IMP-ES FROM UTKORT                                       
112600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
112700         END-IF                                                           
112800     END-EVALUATE                                                         
112900     MOVE 'W46179'               TO POSTSUM-FDNAMN                        
113000     MOVE 'W47618DB'             TO POSTSUM-DDNAMN2                       
113100     CALL POSTSUM      USING POSTSUM-PARM                                 
113200     .                                                                    
113300     EJECT                                                                
113400 S10-SKRIV-W46168 SECTION.                                                
113500*    ÖSTERRIKE                                                            
113600     EVALUATE RIO-IDPTYP                                                  
113700       WHEN 'RIO'                                                         
113800         WRITE SOFT-RIO-POST-AT FROM UTKORT-LONG                          
113900         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
114000       WHEN OTHER                                                         
114100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
114200           WRITE IMP-AT FROM UTKORT-RIM2                                  
114300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
114400         ELSE                                                             
114500           WRITE IMP-AT FROM UTKORT                                       
114600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
114700         END-IF                                                           
114800     END-EVALUATE                                                         
114900     MOVE 'W46168'               TO POSTSUM-FDNAMN                        
115000     MOVE 'W47618DC'             TO POSTSUM-DDNAMN2                       
115100     CALL POSTSUM      USING POSTSUM-PARM                                 
115200     .                                                                    
115300     EJECT                                                                
115400 S10-SKRIV-W46178 SECTION.                                                
115500*    SAUDI-ARABIEN                                                        
115600     EVALUATE RIO-IDPTYP                                                  
115700       WHEN 'RIO'                                                         
115800         PERFORM S30-JA                                                   
115900         WRITE RIO-POST-SA FROM UTKORT-LONG                               
116000         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
116100       WHEN OTHER                                                         
116200         IF WSORT-RIO-IDPTYP = 'RIM'                                      
116300           WRITE IMP-SA FROM UTKORT-RIM2                                  
116400           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
116500         ELSE                                                             
116600           WRITE IMP-SA FROM UTKORT                                       
116700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
116800         END-IF                                                           
116900     END-EVALUATE                                                         
117000     MOVE 'W46178'               TO POSTSUM-FDNAMN                        
117100     MOVE 'W47618DD'             TO POSTSUM-DDNAMN2                       
117200     CALL POSTSUM      USING POSTSUM-PARM                                 
117300     .                                                                    
117400     EJECT                                                                
117500 S10-SKRIV-W46176 SECTION.                                                
117600*    PERU                                                                 
117700     EVALUATE RIO-IDPTYP                                                  
117800       WHEN 'RIO'                                                         
117900         WRITE RIO-POST-PE FROM UTKORT-LONG                               
118000         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
118100       WHEN OTHER                                                         
118200         IF WSORT-RIO-IDPTYP = 'RIM'                                      
118300           WRITE IMP-PE FROM UTKORT-RIM2                                  
118400           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
118500         ELSE                                                             
118600           WRITE IMP-PE FROM UTKORT                                       
118700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
118800         END-IF                                                           
118900     END-EVALUATE                                                         
119000     MOVE 'W46176'               TO POSTSUM-FDNAMN                        
119100     MOVE 'W47618DE'             TO POSTSUM-DDNAMN2                       
119200     CALL POSTSUM      USING POSTSUM-PARM                                 
119300     .                                                                    
119400     EJECT                                                                
119500 S10-SKRIV-W46173 SECTION.                                                
119600*    FRANKRIKE                                                            
119700     EVALUATE RIO-IDPTYP                                                  
119800       WHEN 'RIO'                                                         
119900         WRITE SOFT-RIO-POST-FR FROM UTKORT-LONG                          
120000         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
120100       WHEN OTHER                                                         
120200         IF WSORT-RIO-IDPTYP = 'RIM'                                      
120300           WRITE IMP-FR FROM UTKORT-RIM2                                  
120400           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
120500         ELSE                                                             
120600           WRITE IMP-FR FROM UTKORT                                       
120700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
120800         END-IF                                                           
120900     END-EVALUATE                                                         
121000     MOVE 'W46173'               TO POSTSUM-FDNAMN                        
121100     MOVE 'W47618DF'             TO POSTSUM-DDNAMN2                       
121200     CALL POSTSUM      USING POSTSUM-PARM                                 
121300     .                                                                    
121400     EJECT                                                                
121500 S10-SKRIV-W46170 SECTION.                                                
121600*    SVERIGE                                                              
121700     DISPLAY 'RIO-IDPTYP: ' RIO-IDPTYP                                    
121800     EVALUATE RIO-IDPTYP                                                  
121900       WHEN 'RIO'                                                         
122000         WRITE SOFT-RIO-POST-SE FROM UTKORT-LONG                          
122100         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
122200       WHEN OTHER                                                         
122300         IF WSORT-RIO-IDPTYP = 'RIM'                                      
122400           WRITE IMP-SE FROM UTKORT-RIM2                                  
122500           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
122600         ELSE                                                             
122700           WRITE IMP-SE FROM UTKORT                                       
122800           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
122900         END-IF                                                           
123000     END-EVALUATE                                                         
123100     MOVE 'W46170'               TO POSTSUM-FDNAMN                        
123200     MOVE 'W47618DG'             TO POSTSUM-DDNAMN2                       
123300     CALL POSTSUM      USING POSTSUM-PARM                                 
123400     .                                                                    
123500     EJECT                                                                
123600 S10-SKRIV-W46140 SECTION.                                                
123700*    DANMARK                                                              
123800     EVALUATE RIO-IDPTYP                                                  
123900       WHEN 'RIO'                                                         
124000         WRITE SOFT-RIO-POST-DK FROM UTKORT-LONG                          
124100         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
124200       WHEN OTHER                                                         
124300         IF WSORT-RIO-IDPTYP = 'RIM'                                      
124400           WRITE IMP-DK FROM UTKORT-RIM2                                  
124500           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
124600         ELSE                                                             
124700           WRITE IMP-DK FROM UTKORT                                       
124800           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
124900         END-IF                                                           
125000     END-EVALUATE                                                         
125100     MOVE 'W46140'               TO POSTSUM-FDNAMN                        
125200     MOVE 'W47618DH'             TO POSTSUM-DDNAMN2                       
125300     CALL POSTSUM      USING POSTSUM-PARM                                 
125400     .                                                                    
125500     EJECT                                                                
125600 S10-SKRIV-W46139 SECTION.                                                
125700*    NORGE                                                                
125800     EVALUATE RIO-IDPTYP                                                  
125900       WHEN 'RIO'                                                         
126000         WRITE SOFT-RIO-POST-NO FROM UTKORT-LONG                          
126100         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
126200       WHEN OTHER                                                         
126300         IF WSORT-RIO-IDPTYP = 'RIM'                                      
126400           WRITE IMP-NO FROM UTKORT-RIM2                                  
126500           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
126600         ELSE                                                             
126700           WRITE IMP-NO FROM UTKORT                                       
126800           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
126900         END-IF                                                           
127000     END-EVALUATE                                                         
127100     MOVE 'W46139'               TO POSTSUM-FDNAMN                        
127200     MOVE 'W47618DI'             TO POSTSUM-DDNAMN2                       
127300     CALL POSTSUM      USING POSTSUM-PARM                                 
127400     .                                                                    
127500     EJECT                                                                
127600 S10-SKRIV-W46174 SECTION.                                                
127700*    SCHWEIZ                                                              
127800     EVALUATE RIO-IDPTYP                                                  
127900       WHEN 'RIO'                                                         
128000         WRITE SOFT-RIO-POST-CH2070 FROM UTKORT-LONG                      
128100         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
128200       WHEN OTHER                                                         
128300         IF WSORT-RIO-IDPTYP = 'RIM'                                      
128400           WRITE IMP-CH2070  FROM UTKORT-RIM2                             
128500           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
128600         ELSE                                                             
128700           WRITE IMP-CH2070  FROM UTKORT                                  
128800           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
128900         END-IF                                                           
129000     END-EVALUATE                                                         
129100     MOVE 'W46174'               TO POSTSUM-FDNAMN                        
129200     MOVE 'W47618DJ'             TO POSTSUM-DDNAMN2                       
129300     CALL POSTSUM      USING POSTSUM-PARM                                 
129400     .                                                                    
129500     EJECT                                                                
129600 S10-SKRIV-W46175 SECTION.                                                
129700*    BRAZILIEN                                                            
129800     EVALUATE RIO-IDPTYP                                                  
129900       WHEN 'RIO'                                                         
130000         WRITE RIO-POST-BR FROM UTKORT-LONG                               
130100         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
130200       WHEN OTHER                                                         
130300         IF WSORT-RIO-IDPTYP = 'RIM'                                      
130400           WRITE IMP-BR FROM UTKORT-RIM2                                  
130500           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
130600         ELSE                                                             
130700           WRITE IMP-BR FROM UTKORT                                       
130800           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
130900         END-IF                                                           
131000     END-EVALUATE                                                         
131100     MOVE 'W46175'               TO POSTSUM-FDNAMN                        
131200     MOVE 'W47618DK'             TO POSTSUM-DDNAMN2                       
131300     CALL POSTSUM      USING POSTSUM-PARM                                 
131400     .                                                                    
131500     EJECT                                                                
131600 S10-SKRIV-W46131 SECTION.                                                
131700*    AUSTRALIEN                                                           
131800     EVALUATE RIO-IDPTYP                                                  
131900       WHEN 'RIO'                                                         
132000         PERFORM S30-JA                                                   
132100         WRITE SOFT-RIO-POST-AU7836 FROM UTKORT-LONG                      
132200         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
132300       WHEN OTHER                                                         
132400         IF WSORT-RIO-IDPTYP = 'RIM'                                      
132500           WRITE IMP-AU7836 FROM UTKORT-RIM2                              
132600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
132700         ELSE                                                             
132800           WRITE IMP-AU7836 FROM UTKORT                                   
132900           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
133000         END-IF                                                           
133100     END-EVALUATE                                                         
133200     MOVE 'W46131'               TO POSTSUM-FDNAMN                        
133300     MOVE 'W47618DL'             TO POSTSUM-DDNAMN2                       
133400     CALL POSTSUM      USING POSTSUM-PARM                                 
133500     .                                                                    
133600     EJECT                                                                
133700 S10-SKRIV-W46172 SECTION.                                                
133800*    SCHWEIZ                                                              
133900     EVALUATE RIO-IDPTYP                                                  
134000       WHEN 'RIO'                                                         
134100         WRITE SOFT-RIO-POST-CH2078 FROM UTKORT-LONG                      
134200         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
134300       WHEN OTHER                                                         
134400         IF WSORT-RIO-IDPTYP = 'RIM'                                      
134500           WRITE IMP-CH2078 FROM UTKORT-RIM2                              
134600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
134700         ELSE                                                             
134800           WRITE IMP-CH2078 FROM UTKORT                                   
134900           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
135000         END-IF                                                           
135100     END-EVALUATE                                                         
135200     MOVE 'W46172'               TO POSTSUM-FDNAMN                        
135300     MOVE 'W47618D6'             TO POSTSUM-DDNAMN2                       
135400     CALL POSTSUM      USING POSTSUM-PARM                                 
135500     .                                                                    
135600     EJECT                                                                
135700 S10-SKRIV-W46180 SECTION.                                                
135800*    AUSTRALIEN                                                           
135900     EVALUATE RIO-IDPTYP                                                  
136000       WHEN 'RIO'                                                         
136100         PERFORM S30-JA                                                   
136200         WRITE SOFT-RIO-POST-AU7838 FROM UTKORT-LONG                      
136300         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
136400       WHEN OTHER                                                         
136500         IF WSORT-RIO-IDPTYP = 'RIM'                                      
136600           WRITE IMP-AU7838 FROM UTKORT-RIM2                              
136700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
136800         ELSE                                                             
136900           WRITE IMP-AU7838 FROM UTKORT                                   
137000           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
137100         END-IF                                                           
137200     END-EVALUATE                                                         
137300     MOVE 'W46180'               TO POSTSUM-FDNAMN                        
137400     MOVE 'W47618DM'             TO POSTSUM-DDNAMN2                       
137500     CALL POSTSUM      USING POSTSUM-PARM                                 
137600     .                                                                    
137700     EJECT                                                                
137800 S10-SKRIV-W46134 SECTION.                                                
137900*    TAIWAN                                                               
138000     EVALUATE RIO-IDPTYP                                                  
138100       WHEN 'RIO'                                                         
138200         PERFORM S30-JA                                                   
138300         WRITE SOFT-RIO-POST-TW FROM UTKORT-LONG                          
138400         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
138500       WHEN OTHER                                                         
138600         IF WSORT-RIO-IDPTYP = 'RIM'                                      
138700           WRITE IMP-TW FROM UTKORT-RIM2                                  
138800           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
138900         ELSE                                                             
139000           WRITE IMP-TW FROM UTKORT                                       
139100           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
139200         END-IF                                                           
139300     END-EVALUATE                                                         
139400     MOVE 'W46134'               TO POSTSUM-FDNAMN                        
139500     MOVE 'W47618DO'             TO POSTSUM-DDNAMN2                       
139600     CALL POSTSUM      USING POSTSUM-PARM                                 
139700     .                                                                    
139800     EJECT                                                                
139900 S10-SKRIV-W4612A SECTION.                                                
140000*    TAIWAN2                                                              
140100     EVALUATE RIO-IDPTYP                                                  
140200       WHEN 'RIO'                                                         
140300         PERFORM S30-JA                                                   
140400         WRITE SOFT-RIO-POST-TW2 FROM UTKORT-LONG                         
140500         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
140600       WHEN OTHER                                                         
140700         IF WSORT-RIO-IDPTYP = 'RIM'                                      
140800           WRITE IMP-TW2 FROM UTKORT-RIM2                                 
140900           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
141000         ELSE                                                             
141100           WRITE IMP-TW2 FROM UTKORT                                      
141200           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
141300         END-IF                                                           
141400     END-EVALUATE                                                         
141500     MOVE 'W4612A'               TO POSTSUM-FDNAMN                        
141600     MOVE 'W47618DQ'             TO POSTSUM-DDNAMN2                       
141700     CALL POSTSUM      USING POSTSUM-PARM                                 
141800     .                                                                    
141900     EJECT                                                                
142000 S10-SKRIV-W4612H SECTION.                                                
142100*    JAPAN                                                                
142200     EVALUATE RIO-IDPTYP                                                  
142300       WHEN 'RIO'                                                         
142400         PERFORM S30-JA                                                   
142500         WRITE SOFT-RIO-POST-JP FROM UTKORT-LONG                          
142600         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
142700       WHEN OTHER                                                         
142800         IF WSORT-RIO-IDPTYP = 'RIM'                                      
142900           WRITE IMP-JP FROM UTKORT-RIM2                                  
143000           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
143100         ELSE                                                             
143200           WRITE IMP-JP FROM UTKORT                                       
143300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
143400         END-IF                                                           
143500     END-EVALUATE                                                         
143600     MOVE 'W4612H'               TO POSTSUM-FDNAMN                        
143700     MOVE 'W47618DP'             TO POSTSUM-DDNAMN2                       
143800     CALL POSTSUM      USING POSTSUM-PARM                                 
143900     .                                                                    
144000     EJECT                                                                
144100 S10-SKRIV-W4612J SECTION.                                                
144200*    THAILAND                                                             
144300     EVALUATE RIO-IDPTYP                                                  
144400       WHEN 'RIO'                                                         
144500         PERFORM S30-JA                                                   
144600         WRITE SOFT-RIO-POST-TH FROM UTKORT-LONG                          
144700         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
144800       WHEN OTHER                                                         
144900         IF WSORT-RIO-IDPTYP = 'RIM'                                      
145000           WRITE IMP-TH FROM UTKORT-RIM2                                  
145100           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
145200         ELSE                                                             
145300           WRITE IMP-TH FROM UTKORT                                       
145400           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
145500         END-IF                                                           
145600     END-EVALUATE                                                         
145700     MOVE 'W4612J'               TO POSTSUM-FDNAMN                        
145800     MOVE 'W47618E2'             TO POSTSUM-DDNAMN2                       
145900     CALL POSTSUM      USING POSTSUM-PARM                                 
146000     .                                                                    
146100     EJECT                                                                
146200 S10-SKRIV-W4612I SECTION.                                                
146300*    MALAYSIA                                                             
146400     EVALUATE RIO-IDPTYP                                                  
146500       WHEN 'RIO'                                                         
146600         PERFORM S30-JA                                                   
146700         WRITE SOFT-RIO-POST-MY FROM UTKORT-LONG                          
146800         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
146900       WHEN OTHER                                                         
147000         IF WSORT-RIO-IDPTYP = 'RIM'                                      
147100           WRITE IMP-MY FROM UTKORT-RIM2                                  
147200           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
147300         ELSE                                                             
147400           WRITE IMP-MY FROM UTKORT                                       
147500           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
147600         END-IF                                                           
147700     END-EVALUATE                                                         
147800     MOVE 'W4612I'               TO POSTSUM-FDNAMN                        
147900     MOVE 'W47618E1'             TO POSTSUM-DDNAMN2                       
148000     CALL POSTSUM      USING POSTSUM-PARM                                 
148100     .                                                                    
148200     EJECT                                                                
148300 S10-SKRIV-W46128 SECTION.                                                
148400*    JAPAN LEVANM                                                         
148500     EVALUATE RIO-IDPTYP                                                  
148600       WHEN 'RIO'                                                         
148700         PERFORM S30-JA                                                   
148800         WRITE SOFT-RIO-POST-JP5220 FROM UTKORT-LONG                      
148900         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
149000       WHEN OTHER                                                         
149100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
149200           WRITE IMP-JP5220 FROM UTKORT-RIM2                              
149300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
149400         ELSE                                                             
149500           WRITE IMP-JP5220 FROM UTKORT                                   
149600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
149700         END-IF                                                           
149800     END-EVALUATE                                                         
149900     MOVE 'W46128'               TO POSTSUM-FDNAMN                        
150000     MOVE 'W47618DS'             TO POSTSUM-DDNAMN2                       
150100     CALL POSTSUM      USING POSTSUM-PARM                                 
150200     .                                                                    
150300     EJECT                                                                
150400 S10-SKRIV-W4612B SECTION.                                                
150500*    ENGLAND                                                              
150600     EVALUATE RIO-IDPTYP                                                  
150700       WHEN 'RIO'                                                         
150800         WRITE SOFT-RIO-POST-GB1378 FROM UTKORT-LONG                      
150900         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
151000       WHEN OTHER                                                         
151100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
151200           WRITE IMP-GB1378 FROM UTKORT-RIM2                              
151300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
151400         ELSE                                                             
151500           WRITE IMP-GB1378 FROM UTKORT                                   
151600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
151700         END-IF                                                           
151800     END-EVALUATE                                                         
151900     MOVE 'W4612B'               TO POSTSUM-FDNAMN                        
152000     MOVE 'W47618DT'             TO POSTSUM-DDNAMN2                       
152100     CALL POSTSUM      USING POSTSUM-PARM                                 
152200     .                                                                    
152300     EJECT                                                                
152400 S10-SKRIV-W4612C SECTION.                                                
152500*    POLEN                                                                
152600     EVALUATE RIO-IDPTYP                                                  
152700       WHEN 'RIO'                                                         
152800         WRITE SOFT-RIO-POST-PL FROM UTKORT-LONG                          
152900         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
153000       WHEN OTHER                                                         
153100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
153200           WRITE IMP-PL FROM UTKORT-RIM2                                  
153300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
153400         ELSE                                                             
153500           WRITE IMP-PL FROM UTKORT                                       
153600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
153700         END-IF                                                           
153800     END-EVALUATE                                                         
153900     MOVE 'W4612C'               TO POSTSUM-FDNAMN                        
154000     MOVE 'W47618DU'             TO POSTSUM-DDNAMN2                       
154100     CALL POSTSUM      USING POSTSUM-PARM                                 
154200     .                                                                    
154300     EJECT                                                                
154400 S10-SKRIV-W4612K SECTION.                                                
154500*    IRLAND                                                               
154600     EVALUATE RIO-IDPTYP                                                  
154700       WHEN 'RIO'                                                         
154800         WRITE SOFT-RIO-POST-IE     FROM UTKORT-LONG                      
154900         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
155000       WHEN OTHER                                                         
155100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
155200           WRITE IMP-IE FROM UTKORT-RIM2                                  
155300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
155400         ELSE                                                             
155500           WRITE IMP-IE FROM UTKORT                                       
155600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
155700         END-IF                                                           
155800     END-EVALUATE                                                         
155900     MOVE 'W4612K'               TO POSTSUM-FDNAMN                        
156000     MOVE 'W47618E3'             TO POSTSUM-DDNAMN2                       
156100     CALL POSTSUM      USING POSTSUM-PARM                                 
156200     .                                                                    
156300     EJECT                                                                
156400 S10-SKRIV-W4612L SECTION.                                                
156500*    BRAZILIEN NEW                                                        
156600     EVALUATE RIO-IDPTYP                                                  
156700       WHEN 'RIO'                                                         
156800         WRITE SOFT-RIO-POST-BR-NEW FROM UTKORT-LONG                      
156900         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
157000       WHEN OTHER                                                         
157100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
157200           WRITE IMP-BR-NEW   FROM UTKORT-RIM2                            
157300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
157400         ELSE                                                             
157500           WRITE IMP-BR-NEW   FROM UTKORT                                 
157600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
157700         END-IF                                                           
157800     END-EVALUATE                                                         
157900     MOVE 'W4612L'               TO POSTSUM-FDNAMN                        
158000     MOVE 'W47618D7'             TO POSTSUM-DDNAMN2                       
158100     CALL POSTSUM      USING POSTSUM-PARM                                 
158200     .                                                                    
158300     EJECT                                                                
158400 S10-SKRIV-W4612M SECTION.                                                
158500*    MEXICO                                                               
158600     EVALUATE RIO-IDPTYP                                                  
158700       WHEN 'RIO'                                                         
158800         WRITE SOFT-RIO-POST-MX     FROM UTKORT-LONG                      
158900         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
159000       WHEN OTHER                                                         
159100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
159200           WRITE IMP-MX FROM UTKORT-RIM2                                  
159300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
159400         ELSE                                                             
159500           WRITE IMP-MX FROM UTKORT                                       
159600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
159700         END-IF                                                           
159800     END-EVALUATE                                                         
159900     MOVE 'W4612M'               TO POSTSUM-FDNAMN                        
160000     MOVE 'W47618DZ'             TO POSTSUM-DDNAMN2                       
160100     CALL POSTSUM      USING POSTSUM-PARM                                 
160200     .                                                                    
160300 S10-SKRIV-W4612N SECTION.                                                
160400*    TURKIET                                                              
160500     EVALUATE RIO-IDPTYP                                                  
160600       WHEN 'RIO'                                                         
160700         PERFORM S30-JA                                                   
160800         WRITE SOFT-RIO-POST-TR     FROM UTKORT-LONG                      
160900         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
161000       WHEN OTHER                                                         
161100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
161200           WRITE IMP-TR FROM UTKORT-RIM2                                  
161300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
161400         ELSE                                                             
161500           WRITE IMP-TR FROM UTKORT                                       
161600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
161700         END-IF                                                           
161800     END-EVALUATE                                                         
161900     MOVE 'W4612N'               TO POSTSUM-FDNAMN                        
162000     MOVE 'W47618DZ'             TO POSTSUM-DDNAMN2                       
162100     CALL POSTSUM      USING POSTSUM-PARM                                 
162200     .                                                                    
162300     EJECT                                                                
162400 S10-SKRIV-W4612Z SECTION.                                                
162500*    FINLAND                                                              
162600     EVALUATE RIO-IDPTYP                                                  
162700       WHEN 'RIO'                                                         
162800         WRITE SOFT-RIO-POST-FI1091 FROM UTKORT-LONG                      
162900         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
163000       WHEN OTHER                                                         
163100         IF WSORT-RIO-IDPTYP = 'RIM'                                      
163200           WRITE IMP-FI1091 FROM UTKORT-RIM2                              
163300           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
163400         ELSE                                                             
163500           WRITE IMP-FI1091 FROM UTKORT                                   
163600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
163700         END-IF                                                           
163800     END-EVALUATE                                                         
163900     MOVE 'W4612Z'               TO POSTSUM-FDNAMN                        
164000     MOVE 'W47618DN'             TO POSTSUM-DDNAMN2                       
164100     CALL POSTSUM      USING POSTSUM-PARM                                 
164200     .                                                                    
164300     EJECT                                                                
164400 S10-SKRIV-W4612D SECTION.                                                
164500*    CANADA                                                               
164600     EVALUATE RIO-IDPTYP                                                  
164700       WHEN 'RIO'                                                         
164800         PERFORM S30-JA                                                   
164900         WRITE SOFT-RIO-POST-CAN FROM UTKORT-LONG                         
165000         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
165100       WHEN OTHER                                                         
165200         IF WSORT-RIO-IDPTYP = 'RIM'                                      
165300           WRITE IMP-CAN FROM UTKORT-RIM2                                 
165400           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
165500         ELSE                                                             
165600           WRITE IMP-CAN FROM UTKORT                                      
165700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
165800         END-IF                                                           
165900     END-EVALUATE                                                         
166000                                                                          
166100     MOVE 'W4612D'               TO POSTSUM-FDNAMN                        
166200     MOVE 'W47618DV'             TO POSTSUM-DDNAMN2                       
166300     CALL POSTSUM      USING POSTSUM-PARM                                 
166400     .                                                                    
166500     EJECT                                                                
166600 S10-SKRIV-W4612E SECTION.                                                
166700*    USA                                                                  
166800     EVALUATE RIO-IDPTYP                                                  
166900       WHEN 'RIO'                                                         
167000         PERFORM S30-JA                                                   
167100         WRITE SOFT-RIO-POST-USA FROM UTKORT-LONG                         
167200         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
167300       WHEN OTHER                                                         
167400         IF WSORT-RIO-IDPTYP = 'RIM'                                      
167500           WRITE IMP-USA FROM UTKORT-RIM2                                 
167600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
167700         ELSE                                                             
167800           WRITE IMP-USA FROM UTKORT                                      
167900           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
168000         END-IF                                                           
168100     END-EVALUATE                                                         
168200     MOVE 'W4612E'               TO POSTSUM-FDNAMN                        
168300     MOVE 'W47618DW'             TO POSTSUM-DDNAMN2                       
168400     CALL POSTSUM      USING POSTSUM-PARM                                 
168500     .                                                                    
168600     EJECT                                                                
168700 S10-SKRIV-W4612F SECTION.                                                
168800*    KOREA                                                                
168900     EVALUATE RIO-IDPTYP                                                  
169000       WHEN 'RIO'                                                         
169100         PERFORM S30-JA                                                   
169200         WRITE SOFT-RIO-POST-KR FROM UTKORT-LONG                          
169300         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
169400       WHEN OTHER                                                         
169500         IF WSORT-RIO-IDPTYP = 'RIM'                                      
169600           WRITE IMP-KR FROM UTKORT-RIM2                                  
169700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
169800         ELSE                                                             
169900           WRITE IMP-KR FROM UTKORT                                       
170000           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
170100         END-IF                                                           
170200     END-EVALUATE                                                         
170300     MOVE 'W4612F'               TO POSTSUM-FDNAMN                        
170400     MOVE 'W47618DX'             TO POSTSUM-DDNAMN2                       
170500     CALL POSTSUM      USING POSTSUM-PARM                                 
170600     .                                                                    
170700     EJECT                                                                
170800 S10-SKRIV-W4612G SECTION.                                                
170900*    PORTUGAL                                                             
171000     EVALUATE RIO-IDPTYP                                                  
171100       WHEN 'RIO'                                                         
171200         WRITE SOFT-RIO-POST-PT FROM UTKORT-LONG                          
171300         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
171400       WHEN OTHER                                                         
171500         IF WSORT-RIO-IDPTYP = 'RIM'                                      
171600           WRITE IMP-PT FROM UTKORT-RIM2                                  
171700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
171800         ELSE                                                             
171900           WRITE IMP-PT FROM UTKORT                                       
172000           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
172100         END-IF                                                           
172200     END-EVALUATE                                                         
172300     MOVE 'W4612G'               TO POSTSUM-FDNAMN                        
172400     MOVE 'W47618DY'             TO POSTSUM-DDNAMN2                       
172500     CALL POSTSUM      USING POSTSUM-PARM                                 
172600     .                                                                    
172700     EJECT                                                                
172800 S10-SKRIV-W461RU SECTION.                                                
172900*    RYSSLAND                                                             
173000     EVALUATE RIO-IDPTYP                                                  
173100       WHEN 'RIO'                                                         
173200         WRITE SOFT-RIO-POST-RU     FROM UTKORT-LONG                      
173300         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
173400       WHEN OTHER                                                         
173500         IF WSORT-RIO-IDPTYP = 'RIM'                                      
173600           WRITE IMP-RU FROM UTKORT-RIM2                                  
173700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
173800         ELSE                                                             
173900           WRITE IMP-RU FROM UTKORT                                       
174000           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
174100         END-IF                                                           
174200     END-EVALUATE                                                         
174300     MOVE 'W461RU'               TO POSTSUM-FDNAMN                        
174400     MOVE 'W47618E5'             TO POSTSUM-DDNAMN2                       
174500     CALL POSTSUM      USING POSTSUM-PARM                                 
174600     .                                                                    
174700     EJECT                                                                
174800 S10-SKRIV-W461CN SECTION.                                                
174900*    KINA 6214                                                            
175000     EVALUATE RIO-IDPTYP                                                  
175100       WHEN 'RIO'                                                         
175200         WRITE SOFT-RIO-POST-CN     FROM UTKORT-LONG                      
175300         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
175400       WHEN OTHER                                                         
175500         IF WSORT-RIO-IDPTYP = 'RIM'                                      
175600           WRITE IMP-CN FROM UTKORT-RIM2                                  
175700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
175800         ELSE                                                             
175900           WRITE IMP-CN FROM UTKORT                                       
176000           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
176100         END-IF                                                           
176200     END-EVALUATE                                                         
176300     MOVE 'W461CN'               TO POSTSUM-FDNAMN                        
176400     MOVE 'W47618E6'             TO POSTSUM-DDNAMN2                       
176500     CALL POSTSUM      USING POSTSUM-PARM                                 
176600     .                                                                    
176700     EJECT                                                                
176800 S10-SKRIV-W461CN1 SECTION.                                               
176900*    KINA 6270                                                            
177000     EVALUATE RIO-IDPTYP                                                  
177100       WHEN 'RIO'                                                         
177200         WRITE SOFT-RIO-POST-CN1    FROM UTKORT-LONG                      
177300         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
177400       WHEN OTHER                                                         
177500         IF WSORT-RIO-IDPTYP = 'RIM'                                      
177600           WRITE IMP-CN1 FROM UTKORT-RIM2                                 
177700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
177800         ELSE                                                             
177900           WRITE IMP-CN1 FROM UTKORT                                      
178000           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
178100         END-IF                                                           
178200     END-EVALUATE                                                         
178300     MOVE 'W461CN1'              TO POSTSUM-FDNAMN                        
178400     MOVE 'W47618E9'             TO POSTSUM-DDNAMN2                       
178500     CALL POSTSUM      USING POSTSUM-PARM                                 
178600     .                                                                    
178700     EJECT                                                                
178800 S10-SKRIV-W461ZA SECTION.                                                
178900*    SYDAFRIKA                                                            
179000     EVALUATE RIO-IDPTYP                                                  
179100       WHEN 'RIO'                                                         
179200         WRITE SOFT-RIO-POST-ZA     FROM UTKORT-LONG                      
179300         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
179400       WHEN OTHER                                                         
179500         IF WSORT-RIO-IDPTYP = 'RIM'                                      
179600           WRITE IMP-ZA  FROM UTKORT-RIM2                                 
179700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
179800         ELSE                                                             
179900           WRITE IMP-ZA  FROM UTKORT                                      
180000           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
180100         END-IF                                                           
180200     END-EVALUATE                                                         
180300     MOVE 'W461ZA'               TO POSTSUM-FDNAMN                        
180400     MOVE 'W47618E7'             TO POSTSUM-DDNAMN2                       
180500     CALL POSTSUM      USING POSTSUM-PARM                                 
180600     .                                                                    
180700     EJECT                                                                
180800 S10-SKRIV-W461PT2 SECTION.                                               
180900*    PORTUGAL DIST 1958                                                   
181000     EVALUATE RIO-IDPTYP                                                  
181100       WHEN 'RIO'                                                         
181200         WRITE SOFT-RIO-POST-PT2    FROM UTKORT-LONG                      
181300         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
181400       WHEN OTHER                                                         
181500         IF WSORT-RIO-IDPTYP = 'RIM'                                      
181600           WRITE IMP-PT2 FROM UTKORT-RIM2                                 
181700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
181800         ELSE                                                             
181900           WRITE IMP-PT2 FROM UTKORT                                      
182000           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
182100         END-IF                                                           
182200     END-EVALUATE                                                         
182300     MOVE 'W461PT2'              TO POSTSUM-FDNAMN                        
182400     MOVE 'W47618E8'             TO POSTSUM-DDNAMN2                       
182500     CALL POSTSUM      USING POSTSUM-PARM                                 
182600     .                                                                    
182700     EJECT                                                                
182800 S10-SKRIV-W461IN SECTION.                                                
182900*    INDIEN                                                               
183000     EVALUATE RIO-IDPTYP                                                  
183100       WHEN 'RIO'                                                         
183200         WRITE SOFT-RIO-POST-IN FROM UTKORT-LONG                          
183300         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
183400       WHEN OTHER                                                         
183500         IF RIM-IDPTYP = 'RIM'                                            
183600           WRITE IMP-IN FROM UTKORT-RIM2                                  
183700           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
183800         ELSE                                                             
183900           WRITE IMP-IN FROM UTKORT                                       
184000           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
184100         END-IF                                                           
184200     END-EVALUATE                                                         
184300     MOVE 'W461IN'               TO POSTSUM-FDNAMN                        
184400     MOVE 'W47618EA'             TO POSTSUM-DDNAMN2                       
184500     CALL POSTSUM      USING POSTSUM-PARM                                 
184600     .                                                                    
184700 S10-SKRIV-W461CZ SECTION.                                                
184800*    TJECKIEN                                                             
184900     EVALUATE RIO-IDPTYP                                                  
185000       WHEN 'RIO'                                                         
185100         WRITE SOFT-RIO-POST-CZ FROM UTKORT-LONG                          
185200         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
185300       WHEN OTHER                                                         
185400         IF RIM-IDPTYP = 'RIM'                                            
185500           WRITE IMP-CZ FROM UTKORT-RIM2                                  
185600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
185700         ELSE                                                             
185800           WRITE IMP-CZ FROM UTKORT                                       
185900           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
186000         END-IF                                                           
186100     END-EVALUATE                                                         
186200     MOVE 'W461CZ'               TO POSTSUM-FDNAMN                        
186300     MOVE 'W47618EB'             TO POSTSUM-DDNAMN2                       
186400     CALL POSTSUM      USING POSTSUM-PARM                                 
186500     .                                                                    
186600     EJECT                                                                
186700 S10-SKRIV-W461HU SECTION.                                                
186800*    UNGERN                                                               
186900     EVALUATE RIO-IDPTYP                                                  
187000       WHEN 'RIO'                                                         
187100         WRITE SOFT-RIO-POST-HU FROM UTKORT-LONG                          
187200         MOVE SPAR-IDPTYP TO POSTSUM-TRANSTYP                             
187300       WHEN OTHER                                                         
187400         IF RIM-IDPTYP = 'RIM'                                            
187500           WRITE IMP-HU FROM UTKORT-RIM2                                  
187600           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
187700         ELSE                                                             
187800           WRITE IMP-HU FROM UTKORT                                       
187900           MOVE RIK-IDPTYP TO POSTSUM-TRANSTYP                            
188000         END-IF                                                           
188100     END-EVALUATE                                                         
188200     MOVE 'W461HU'               TO POSTSUM-FDNAMN                        
188300     MOVE 'W47618EC'             TO POSTSUM-DDNAMN2                       
188400     CALL POSTSUM      USING POSTSUM-PARM                                 
188500     .                                                                    
188600     EJECT                                                                
188700 S30-JA  SECTION.                                                         
188800     IF SOFT-RIO-FLINVEST = 'J'                                           
188900       MOVE 'Y'                  TO SOFT-RIO-FLINVEST                     
189000     END-IF                                                               
189100     IF SOFT-RIO-FLPRTILL = 'J'                                           
189200       MOVE 'Y'                  TO SOFT-RIO-FLPRTILL                     
189300     END-IF                                                               
189400     IF SOFT-RIO-FLDIRLEV = 'J'                                           
189500       MOVE 'Y'                  TO SOFT-RIO-FLDIRLEV                     
189600     END-IF                                                               
189700     .                                                                    
189800     EJECT                                                                
189900 Z-FINIT SECTION.                                                         
190000     CLOSE INFIL                                                          
190100     CLOSE                                                                
190200     W46128                                                               
190300     W46127                                                               
190400     W46130                                                               
190500     W46129 W4612Z                                                        
190600     W46131                                                               
190700     W46172                                                               
190800     W46138                                                               
190900     W46171                                                               
191000     W46179                                                               
191100     W46168                                                               
191200     W46161                                                               
191300     W46176                                                               
191400     W46178                                                               
191500     W46139                                                               
191600     W46170                                                               
191700     W46140                                                               
191800     W46173                                                               
191900     W46174                                                               
192000     W46175                                                               
192100     W46180 W46134                                                        
192200     W4612G                                                               
192300     W4612K W4612B                                                        
192400     W4612A                                                               
192500     W4612C                                                               
192600     W4612D W4612E                                                        
192700     W4612F                                                               
192800     W4612H                                                               
192900     W4612I W4612J                                                        
193000     W4612L W4612M                                                        
193100     W4612N                                                               
193200     W461RU                                                               
193300     W461CN                                                               
193400     W461CN1                                                              
193500     W461ZA                                                               
193600     W461PT2                                                              
193700     W461IN                                                               
193800     W461CZ W461HU                                                        
193900                                                                          
194000     MOVE 'S' TO POSTSUM-OPKOD                                            
194100     CALL POSTSUM USING POSTSUM-PARM                                      
194200     .                                                                    
