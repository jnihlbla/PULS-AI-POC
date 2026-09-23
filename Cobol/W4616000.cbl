000100 ID  DIVISION.                                                            
000200     SKIP3                                                                
000300 PROGRAM-ID.    W4616000.                                                 
000400*AUTHOR.        STIG MULLER.                                              
000500*DATE-WRITTEN.  NOV  1984.                                                
000600                                                                          
000700*                                                                         
000800*    REMARKS.                                                             
000900*    FUNKTION:                                                            
001000*                                                                         
001100*    ABENDKODER:                                                          
001200*                                                                         
001300*        U0016    - OM RETURKOD FRÅN SORT                                 
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*- - - - - - - - - - - - INFILER:                                         
002200     SELECT W46119                       ASSIGN TO W46160D1.              
002300     SELECT REG-IN                       ASSIGN TO W46160D2.              
002400     SKIP2                                                                
002500*- - - - - - - - - - - - UTFILER:                                         
002600     SELECT W4616G                       ASSIGN TO W46160D3.              
002700     SELECT W4616I                       ASSIGN TO W46160D4.              
002800     SELECT W4616J                       ASSIGN TO W46160D5.              
002900     SELECT W4616N                       ASSIGN TO W46160D6.              
003000     SELECT REG-UT                       ASSIGN TO W46160D7.              
003100     SELECT W4616P                       ASSIGN TO W46160D8.              
003200     SELECT W4616Q                       ASSIGN TO W46160D9.              
003300     SELECT W4616R                       ASSIGN TO W46160DA.              
003400     SELECT W4616S                       ASSIGN TO W46160DB.              
003500     SELECT W4616T                       ASSIGN TO W46160DC.              
003600     SELECT W4616U                       ASSIGN TO W46160DD.              
003700     SELECT W4616V                       ASSIGN TO W46160DE.              
003800     SELECT W4616W                       ASSIGN TO W46160DF.              
003900     SELECT W4617J                       ASSIGN TO W46160DG.              
004000     SELECT W4616K                       ASSIGN TO W46160DH.              
004100     SELECT W46162                       ASSIGN TO W46160DI.              
004200     SELECT W4617A                       ASSIGN TO W46160DJ.              
004300     SELECT W4617B                       ASSIGN TO W46160DK.              
004400     SELECT W4617C                       ASSIGN TO W46160DL.              
004500     SELECT W4617H                       ASSIGN TO W46160DM.              
004600     SELECT W4617G                       ASSIGN TO W46160DN.              
004700     SELECT W4617L                       ASSIGN TO W46160DO.              
004800     SELECT W4617M                       ASSIGN TO W46160DP.              
004900     SELECT W4617P                       ASSIGN TO W46160DR.              
005000     SELECT W4617Q                       ASSIGN TO W46160DS.              
005100     SELECT W4618E                       ASSIGN TO W46160DT.              
005200     SELECT W4618F                       ASSIGN TO W46160DU.              
005300     SELECT W4618G                       ASSIGN TO W46160DV.              
005400     SELECT W4618A                       ASSIGN TO W46160DW.              
005500     SELECT W4618H                       ASSIGN TO W46160DX.              
005600     SELECT W4618I                       ASSIGN TO W46160DY.              
005700     SELECT W4618J                       ASSIGN TO W46160DZ.              
005800     SELECT W4618K                       ASSIGN TO W46160EA.              
005900     SELECT W4618L                       ASSIGN TO W46160EB.              
006000     EJECT                                                                
006100 DATA DIVISION.                                                           
006200     SKIP2                                                                
006300 FILE SECTION.                                                            
006400     SKIP3                                                                
006500 FD  W46119                                                               
006600     RECORDING      F                                                     
006700     BLOCK CONTAINS 0.                                                    
006800     SKIP2                                                                
006900*01  FILLER -COPY W4611901   -L.                                          
007000     EJECT                                                                
007100 FD  REG-IN                                                               
007200     RECORDING       F                                                    
007300     BLOCK CONTAINS 0.                                                    
007400     SKIP2                                                                
007500*01  FILLER -COPY W461050    -L.                                          
007600     EJECT                                                                
007700 FD  W4616G                                                               
007800     RECORDING       V                                                    
007900     BLOCK CONTAINS 0.                                                    
008000     SKIP1                                                                
008100 01  ITALY-POST               PIC X(80).                                  
008200     SKIP3                                                                
008300 FD  W4616I                                                               
008400     RECORDING       V                                                    
008500     BLOCK CONTAINS 0.                                                    
008600     SKIP1                                                                
008700 01  FINL-POST                PIC X(80).                                  
008800     SKIP2                                                                
008900 FD  W4616J                                                               
009000     RECORDING       V                                                    
009100     BLOCK CONTAINS 0.                                                    
009200     SKIP1                                                                
009300 01  BELG-POST                PIC X(80).                                  
009400     SKIP2                                                                
009500 FD  W4616N                                                               
009600     RECORDING       V                                                    
009700     BLOCK CONTAINS 0.                                                    
009800     SKIP1                                                                
009900 01  PORT-POST                PIC X(80).                                  
010000     SKIP2                                                                
010100 FD  W4616P                                                               
010200     RECORDING       V                                                    
010300     BLOCK CONTAINS 0.                                                    
010400     SKIP1                                                                
010500 01  JAPAN-POST               PIC X(80).                                  
010600     SKIP2                                                                
010700 FD  W4616Q                                                               
010800     RECORDING       V                                                    
010900     BLOCK CONTAINS 0.                                                    
011000     SKIP1                                                                
011100 01  ENGLAND-POST             PIC X(80).                                  
011200     SKIP2                                                                
011300 FD  W4616R                                                               
011400     RECORDING       V                                                    
011500     BLOCK CONTAINS 0.                                                    
011600     SKIP1                                                                
011700 01  USA-POST                 PIC X(80).                                  
011800     SKIP2                                                                
011900 FD  W4616S                                                               
012000     RECORDING       V                                                    
012100     BLOCK CONTAINS 0.                                                    
012200     SKIP1                                                                
012300 01  HOLLAND-POST             PIC X(80).                                  
012400     SKIP2                                                                
012500 FD  W4616T                                                               
012600     RECORDING       V                                                    
012700     BLOCK CONTAINS 0.                                                    
012800     SKIP1                                                                
012900 01  SWEDEN-POST              PIC X(80).                                  
013000     SKIP2                                                                
013100 FD  W4616U                                                               
013200     RECORDING       V                                                    
013300     BLOCK CONTAINS 0.                                                    
013400     SKIP1                                                                
013500 01  DENMARK-POST             PIC X(80).                                  
013600     SKIP2                                                                
013700 FD  W4616V                                                               
013800     RECORDING       V                                                    
013900     BLOCK CONTAINS 0.                                                    
014000     SKIP1                                                                
014100 01  NORWAY-POST              PIC X(80).                                  
014200     SKIP2                                                                
014300 FD  W4616W                                                               
014400     RECORDING       V                                                    
014500     BLOCK CONTAINS 0.                                                    
014600     SKIP1                                                                
014700 01  SWISS-IMP-POST           PIC X(80).                                  
014800     SKIP2                                                                
014900 FD  W4617J                                                               
015000     RECORDING       V                                                    
015100     BLOCK CONTAINS 0.                                                    
015200     SKIP1                                                                
015300 01  SWISS-NC-POST            PIC X(80).                                  
015400     SKIP2                                                                
015500 FD  W4616K                                                               
015600     RECORDING       V                                                    
015700     BLOCK CONTAINS 0.                                                    
015800     SKIP1                                                                
015900 01  AUST-7836-POST           PIC X(80).                                  
016000     SKIP2                                                                
016100 FD  W46162                                                               
016200     RECORDING       V                                                    
016300     BLOCK CONTAINS 0.                                                    
016400     SKIP1                                                                
016500 01  GERMANY-POST             PIC X(80).                                  
016600     SKIP2                                                                
016700 FD  W4617A                                                               
016800     RECORDING       V                                                    
016900     BLOCK CONTAINS 0.                                                    
017000     SKIP1                                                                
017100 01  FRANCE-POST              PIC X(80).                                  
017200     SKIP2                                                                
017300 FD  W4617B                                                               
017400     RECORDING       V                                                    
017500     BLOCK CONTAINS 0.                                                    
017600     SKIP1                                                                
017700 01  SPAIN-POST               PIC X(80).                                  
017800     SKIP2                                                                
017900 FD  W4617C                                                               
018000     RECORDING       V                                                    
018100     BLOCK CONTAINS 0.                                                    
018200     SKIP1                                                                
018300 01  AUSTRIA-POST             PIC X(80).                                  
018400     SKIP2                                                                
018500 FD  W4617H                                                               
018600     RECORDING       V                                                    
018700     BLOCK CONTAINS 0.                                                    
018800     SKIP1                                                                
018900 01  BRAZIL-POST              PIC X(80).                                  
019000     SKIP2                                                                
019100 FD  W4617G                                                               
019200     RECORDING       V                                                    
019300     BLOCK CONTAINS 0.                                                    
019400     SKIP1                                                                
019500 01  PERU-POST                PIC X(80).                                  
019600     SKIP2                                                                
019700 FD  W4617L                                                               
019800     RECORDING       V                                                    
019900     BLOCK CONTAINS 0.                                                    
020000     SKIP1                                                                
020100 01  AUST-7838-POST           PIC X(80).                                  
020200     SKIP2                                                                
020300 FD  W4617M                                                               
020400     RECORDING       V                                                    
020500     BLOCK CONTAINS 0.                                                    
020600     SKIP1                                                                
020700 01  TAIWAN-POST              PIC X(80).                                  
020800     SKIP2                                                                
020900 FD  W4617P                                                               
021000     RECORDING       V                                                    
021100     BLOCK CONTAINS 0.                                                    
021200     SKIP1                                                                
021300 01  THAILAND-POST            PIC X(80).                                  
021400     SKIP2                                                                
021500 FD  W4617Q                                                               
021600     RECORDING       V                                                    
021700     BLOCK CONTAINS 0.                                                    
021800     SKIP1                                                                
021900 01  MALAYSIA-POST            PIC X(80).                                  
022000     SKIP2                                                                
022100 FD  W4618E                                                               
022200     RECORDING       V                                                    
022300     BLOCK CONTAINS 0.                                                    
022400     SKIP1                                                                
022500 01  SAUDI-POST               PIC X(80).                                  
022600     SKIP2                                                                
022700 FD  W4618F                                                               
022800     RECORDING       V                                                    
022900     BLOCK CONTAINS 0.                                                    
023000     SKIP1                                                                
023100 01  ENGLAND-NC-POST          PIC X(80).                                  
023200     SKIP2                                                                
023300 FD  W4618G                                                               
023400     RECORDING       V                                                    
023500     BLOCK CONTAINS 0.                                                    
023600     SKIP1                                                                
023700 01  POLEN-POST               PIC X(80).                                  
023800     SKIP2                                                                
023900 FD  W4618A                                                               
024000     RECORDING       V                                                    
024100     BLOCK CONTAINS 0.                                                    
024200     SKIP1                                                                
024300 01  FI-1091-POST             PIC X(80).                                  
024400     SKIP2                                                                
024500 FD  W4618H                                                               
024600     RECORDING       V                                                    
024700     BLOCK CONTAINS 0.                                                    
024800     SKIP1                                                                
024900 01  CAN-POST                 PIC X(80).                                  
025000     SKIP2                                                                
025100 FD  W4618I                                                               
025200     RECORDING       V                                                    
025300     BLOCK CONTAINS 0.                                                    
025400     SKIP1                                                                
025500 01  RYSS-POST                PIC X(80).                                  
025600     SKIP2                                                                
025700 FD  W4618J                                                               
025800     RECORDING       V                                                    
025900     BLOCK CONTAINS 0.                                                    
026000     SKIP1                                                                
026100 01  KINA-POST                PIC X(80).                                  
026200     SKIP2                                                                
026300 FD  W4618K                                                               
026400     RECORDING       V                                                    
026500     BLOCK CONTAINS 0.                                                    
026600     SKIP1                                                                
026700 01  SYDAF-POST               PIC X(80).                                  
026800     SKIP2                                                                
026900 FD  W4618L                                                               
027000     RECORDING       V                                                    
027100     BLOCK CONTAINS 0.                                                    
027200     SKIP1                                                                
027300 01  PORTU-POST               PIC X(80).                                  
027400     SKIP2                                                                
027500 FD  REG-UT                                                               
027600     RECORDING       F                                                    
027700     BLOCK CONTAINS 0.                                                    
027800     SKIP2                                                                
027900*01  REG-UT-POST -COPY W461050    -L.                                     
028000     EJECT                                                                
028100 WORKING-STORAGE SECTION.                                                 
028200*    -COPY WY2000W9                                                       
028300     SKIP3                                                                
028400*- - - - - - - - - - - - - -   PROGRAM-NAMN                               
028500 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4616000'.            
028600     SKIP2                                                                
028700*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
028800                                                                          
028900 77  JA                          PIC X(1)    VALUE 'J'.                   
029000 77  NEJ                         PIC X(1)    VALUE 'N'.                   
029100     SKIP2                                                                
029200*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
029300                                                                          
029400 77  W46119-EOF                  PIC X(1)    VALUE 'N'.                   
029500 77  REG-IN-EOF                  PIC X(1)    VALUE 'N'.                   
029600     SKIP2                                                                
029700*- - - - - - - - - - - - - -  DIVERSE VARIABLER                           
029800 77  BEARBETNING                 PIC X(1).                                
029900 77  ART-ANDRAD                  PIC X(1).                                
030000 77  VECKOR-TILL-TIFINLV         PIC S9(3) COMP-3 VALUE ZERO.             
030100 77  AAR-SEDAN-TIERSDAT          PIC S9(3) COMP-3 VALUE ZERO.             
030200 77  IX                          PIC S9(2) COMP-3 VALUE ZERO.             
030300 77  IX-RAD                      PIC S9(5) COMP-3 VALUE ZERO.             
030400 77  IX-KOLUMN                   PIC S9(5) COMP-3 VALUE ZERO.             
030500 77  IX-STATNR                   PIC S9(3) COMP-3 VALUE ZERO.             
030600 77  KDHBLKRV                    PIC S9(5) COMP-3.                        
030700 77  W-MARKNAD                   PIC X(1)  VALUE SPACE.                   
030800 77  POST-RAKNARE                PIC S9(9) COMP-3 VALUE ZERO.             
030900 77  POST-MAX                    PIC S9(9) COMP-3 VALUE 999999.           
031000     EJECT                                                                
031100*- - - - - - - - - - - - - -  ÅR-VECKA                                    
031200 01  AAR-VECKA                   PIC 9(5).                                
031300 01  FILLER REDEFINES AAR-VECKA.                                          
031400     03  AAR                     PIC 99.                                  
031500     03  VECKA                   PIC 99.                                  
031600     03  FILLER                  PIC  9.                                  
031700     SKIP3                                                                
031800 01  AA53D.                                                               
031900     03  AA                      PIC 99.                                  
032000     03  FILLER                  PIC 999 VALUE 531.                       
032100     SKIP3                                                                
032200 01  SPRAK-AREA.                                                          
032300   02  SPRAK-BEART.                                                       
032400     03  SPRAK-TYSK-BEART            PIC X(25).                           
032500     03  SPRAK-SPANSK-BEART          PIC X(25).                           
032600     03  SPRAK-FRANSK-BEART          PIC X(25).                           
032700     03  SPRAK-ENGELSK-BEART         PIC X(25).                           
032800     03  SPRAK-ITALIENSK-BEART       PIC X(25).                           
032900     03  SPRAK-HOLLANDSK-BEART       PIC X(25).                           
033000     03  SPRAK-PORTUGISISK-BEART     PIC X(25).                           
033100     03  SPRAK-SVENSK-BEART          PIC X(25).                           
033200     03  SPRAK-FINSK-BEART           PIC X(25).                           
033300     03  SPRAK-AMERIKANSK-BEART      PIC X(25).                           
033400     EJECT                                                                
033500*      --- VALID IDDC CODES                                               
033600*                                                                         
033700*01    -COPY WWDCKONS                                                     
033800       EJECT                                                              
033900                                                                          
034000*01    -COPY WWPRODSL                                                     
034100       EJECT                                                              
034200                                                                          
034300 01  DYNAMISKA-SUBPROGRAM.                                                
034400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
034500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
034600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
034700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
034800     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
034900     SKIP3                                                                
035000*- - - - - - - - - - - - - -  KDSRA-TABELL                                
035100 01  KDSRA-TABELL-VARDEN.                                                 
035200     03  FILLER               PIC X(32) VALUE                             
035300     '00000000012001200003434001234340'.                                  
035400     SKIP2                                                                
035500 01  KDSRA-TABELL REDEFINES KDSRA-TABELL-VARDEN.                          
035600     03 RAD OCCURS 4.                                                     
035700        05 KOLUMN OCCURS 8.                                               
035800           07 KDSRA-VARDE  PIC X.                                         
035900     SKIP3                                                                
036000*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
036100                                                                          
036200 01  RETURKODER.                                                          
036300     03  RKOD                    PIC S9(4)  COMP SYNC VALUE ZERO.         
036400     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4)  COMP SYNC VALUE +16.          
036500     03  RKOD-ABEND-MED-DUMP     PIC S9(4)  COMP SYNC VALUE +1000.        
036600     EJECT                                                                
036700*- - - - - - - - - - - - - - - - PARAMETRAR TILL DATKORT                  
036800*                                                                         
036900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
037000     SKIP2                                                                
037100*01  -COPY WDATKORT                                                       
037200     EJECT                                                                
037300*- - - - - - - - - - - - - - - - PARAMETRAR TILL WDATKONV                 
037400*                                                                         
037500*01  -COPY WDATAREA                                                       
037600     EJECT                                                                
037700*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
037800                                                                          
037900*01  -COPY W0005       -PRE  POSTSUM-.                                    
038000     EJECT                                                                
038100*- - - - - - - - - - - - - -  PARAMETRAR TILL W400ARTU                    
038200                                                                          
038300*01  -COPY W400ARTU                                                       
038400     EJECT                                                                
038500******************************************************************        
038600*         W46119-AREA                                            *        
038700******************************************************************        
038800 01  W46119-AREA.                                                         
038900*    03  FILLER -COPY W4611901   -PRE W46119-.                            
039000     EJECT                                                                
039100******************************************************************        
039200*         REG-IN-AREA                                            *        
039300******************************************************************        
039400 01  REG-IN-AREA.                                                         
039500*    03  FILLER -COPY W461050    -PRE REG-IN-.                            
039600     EJECT                                                                
039700******************************************************************        
039800*         RIS-AREA                                               *        
039900******************************************************************        
040000*01  RIS-AREA -COPY W461RISN.                                             
040100     EJECT                                                                
040200******************************************************************        
040300*         RKA-AREA                                               *        
040400******************************************************************        
040500*01  RKA-AREA -COPY W461RKA0.                                             
040600     EJECT                                                                
040700******************************************************************        
040800*         REG-UT-AREA                                            *        
040900******************************************************************        
041000 01  REG-UT-AREA.                                                         
041100*    03  FILLER -COPY W461050    -PRE REG-UT-.                            
041200     EJECT                                                                
041300                                                                          
041400 PROCEDURE DIVISION.                                                      
041500                                                                          
041600     PERFORM A-INIT                                                       
041700     PERFORM S01-LAS-W46119                                               
041800     PERFORM S02-LAS-REG-IN                                               
041900     PERFORM UNTIL W46119-EOF = JA                                        
042000       PERFORM B-SOLLA-POSTER                                             
042100       IF BEARBETNING = JA                                                
042200         IF W46119-IDARTNR = REG-IN-ART-IDARTNR                           
042300           MOVE NEJ TO ART-ANDRAD                                         
042400           PERFORM D-JAMFOR-FILER                                         
042500           IF ART-ANDRAD = JA                                             
042600             PERFORM S12-HAMTA-BENAMNING                                  
042700             PERFORM C-FLYTTA-FAELT                                       
042800             PERFORM S10-SKRIV-REG-UT                                     
042900             PERFORM S11-SKRIV-TILL-IMPORTOR                              
043000             PERFORM S01-LAS-W46119                                       
043100             PERFORM S02-LAS-REG-IN                                       
043200           ELSE                                                           
043300             PERFORM C-FLYTTA-FAELT                                       
043400             PERFORM S10-SKRIV-REG-UT                                     
043500             PERFORM S01-LAS-W46119                                       
043600             PERFORM S02-LAS-REG-IN                                       
043700           END-IF                                                         
043800         ELSE                                                             
043900           EVALUATE TRUE                                                  
044000           WHEN W46119-IDARTNR < REG-IN-ART-IDARTNR                       
044100             PERFORM S12-HAMTA-BENAMNING                                  
044200             PERFORM C-FLYTTA-FAELT                                       
044300             PERFORM S10-SKRIV-REG-UT                                     
044400             PERFORM S11-SKRIV-TILL-IMPORTOR                              
044500             PERFORM S01-LAS-W46119                                       
044600           WHEN W46119-IDARTNR > REG-IN-ART-IDARTNR                       
044700             PERFORM S02-LAS-REG-IN                                       
044800           END-EVALUATE                                                   
044900         END-IF                                                           
045000       ELSE                                                               
045100         PERFORM S01-LAS-W46119                                           
045200       END-IF                                                             
045300     END-PERFORM                                                          
045400     PERFORM Z-FINIT                                                      
045500     IF POST-RAKNARE > POST-MAX                                           
045600       MOVE +2   TO RETURN-CODE                                           
045700     ELSE                                                                 
045800       MOVE ZERO TO RETURN-CODE                                           
045900     END-IF                                                               
046000     GOBACK                                                               
046100     .                                                                    
046200     EJECT                                                                
046300 A-INIT SECTION.                                                          
046400     SKIP2                                                                
046500     OPEN INPUT  REG-IN W46119                                            
046600     OPEN OUTPUT REG-UT                                                   
046700                 W4616G                                                   
046800                 W4616I                                                   
046900                 W4616J                                                   
047000                 W4616N                                                   
047100                 W4616P                                                   
047200                 W4616Q                                                   
047300                 W4616R                                                   
047400                 W4616S                                                   
047500                 W4616T                                                   
047600                 W4616U                                                   
047700                 W4616V                                                   
047800                 W4616W                                                   
047900                 W4617J                                                   
048000                 W4616K                                                   
048100                 W46162                                                   
048200                 W4617A                                                   
048300                 W4617B                                                   
048400                 W4617C                                                   
048500                 W4617H                                                   
048600                 W4617G                                                   
048700                 W4617L                                                   
048800                 W4617M                                                   
048900                 W4617P                                                   
049000                 W4617Q                                                   
049100                 W4618E                                                   
049200                 W4618F                                                   
049300                 W4618G                                                   
049400                 W4618A                                                   
049500                 W4618H                                                   
049600                 W4618I                                                   
049700                 W4618J                                                   
049800                 W4618K                                                   
049900                 W4618L                                                   
050000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
050100     MOVE ZERO          TO POST-RAKNARE                                   
050200     .                                                                    
050300     EJECT                                                                
050400 B-SOLLA-POSTER SECTION.                                                  
050500     SKIP2                                                                
050600******************************************************************        
050700*                                                                         
050800*    SELEKTERAR BORT FÖLJANDE POSTER:                                     
050900*    -OM MER ÄN 5 VECKOR TILL FÖRSTA INLEVERANS                           
051000*    -OM KDUART = 'S' ELLER 'M'                                           
051100*    -OM FLLSRDEL = 'N'                                                   
051200*    -OM KDPRODSL = 0 ELLER > 90                                          
051300*    -OM IDFKNGRP = 0                                                     
051400*    -OM KDERS     > 20 OCH TIERSDAT ÄLDRE ÄN 4 ÅR                        
051500*                                                                         
051600******************************************************************        
051700     SKIP2                                                                
051800     MOVE W46119-TIFINLV TO AAR-VECKA                                     
051900     MOVE AAR     TO TMP1-YY                                              
052000     MOVE D-AAR   TO TMP2-YY                                              
052100     PERFORM WY2000P9                                                     
052200     IF TMP1-YY > TMP2-YY                                                 
052300       ADD VECKA TO VECKOR-TILL-TIFINLV                                   
052400       MOVE AAR     TO AA                                                 
052500       MOVE AA53D   TO DAT-I-TIDATUM                                      
052600       MOVE 'AAVVD' TO DAT-KDDATFORM                                      
052700       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
052800                           DAT-O-TIDATUM DAT-KDSVAR                       
052900       IF DAT-KDSVAR-FEL                                                  
053000         COMPUTE VECKOR-TILL-TIFINLV = VECKOR-TILL-TIFINLV +              
053100                 52 - D-VECKA                                             
053200       ELSE                                                               
053300         COMPUTE VECKOR-TILL-TIFINLV = VECKOR-TILL-TIFINLV +              
053400                 53 - D-VECKA                                             
053500       END-IF                                                             
053600     ELSE                                                                 
053700       EVALUATE TRUE                                                      
053800       WHEN AAR = D-AAR                                                   
053900         COMPUTE VECKOR-TILL-TIFINLV = VECKA - D-VECKA                    
054000        WHEN OTHER                                                        
054100         MOVE 0 TO VECKOR-TILL-TIFINLV                                    
054200       END-EVALUATE                                                       
054300     END-IF                                                               
054400     IF W46119-TIERSDAT = 0                                               
054500       MOVE 0 TO AAR-SEDAN-TIERSDAT                                       
054600     ELSE                                                                 
054700       MOVE W46119-TIERSDAT TO AAR-VECKA                                  
054800       MOVE D-AAR TO TMP1-YY                                              
054900       MOVE AAR   TO TMP2-YY                                              
055000       PERFORM WY2000P9                                                   
055100       COMPUTE AAR-SEDAN-TIERSDAT = TMP1-YY - TMP2-YY                     
055200       IF VECKA < D-VECKA                                                 
055300         ADD 1 TO AAR-SEDAN-TIERSDAT                                      
055400       END-IF                                                             
055500     END-IF                                                               
055600     MOVE JA TO BEARBETNING                                               
055700     MOVE W46119-KDPRODSL        TO TEST-KDPRODSL                         
055800     IF VECKOR-TILL-TIFINLV > 5                                           
055900     OR W46119-KDERS    = 52                                              
056000     OR W46119-KDUART = 'S' OR 'M'                                        
056100     OR W46119-FLLSRDEL = 'N'                                             
056200     OR W46119-IDFKNGRP = 0                                               
056300     OR W46119-KDPRODSL = 0                                               
056400     OR KDPRODSL-LOCAL                                                    
056500     OR W46119-IDLKTO = 314390                                            
056600     OR                 314394                                            
056700     OR                 314399                                            
056800     OR                 314651                                            
056900     OR (W46119-KDERS    > 20 AND AAR-SEDAN-TIERSDAT > 4)                 
057000       MOVE NEJ TO BEARBETNING                                            
057100     END-IF                                                               
057200     .                                                                    
057300     EJECT                                                                
057400 C-FLYTTA-FAELT SECTION.                                                  
057500     SKIP2                                                                
057600     MOVE 'RIS'                  TO RIS-IDPTYP                            
057700     MOVE '050'                  TO REG-UT-ART-IDPTYP                     
057800     MOVE W46119-IDARTNR         TO RIS-IDARTNR                           
057900                                    REG-UT-ART-IDARTNR                    
058000     MOVE W46119-REKSIFFR        TO RIS-REKSIFFR                          
058100                                    REG-UT-ART-REKSIFFR                   
058200     MOVE W46119-IDFKNGRP        TO RIS-IDFKNGRP                          
058300                                    REG-UT-ART-IDFKNGRP                   
058400     MOVE W46119-KDSRA           TO RIS-KDSRA                             
058500                                    REG-UT-ART-KDSRA                      
058600     MOVE W46119-KVQPACK-1       TO RIS-KVQPACK-1                         
058700                                    REG-UT-ART-KVQPACK-1                  
058800     MOVE     WC-CDC-SE          TO RIS-IDDC                              
058900     MOVE W46119-KDARTURS        TO REG-UT-ART-KDARTURS                   
059000                                                                          
059100     PERFORM CA-AENDRA-KDARTURS-TILL-NUM                                  
059200     MOVE ARTU-KDARTURS-NUM      TO RIS-KDARTURS                          
059300                                                                          
059400     MOVE W46119-KDPRODSL        TO RIS-KDPRODSL                          
059500                                    REG-UT-ART-KDPRODSL                   
059600     MOVE W46119-VLARTNTO        TO RIS-VLARTNTO                          
059700                                    REG-UT-ART-VLARTNTO                   
059800     MOVE W46119-VKART           TO RIS-VKART                             
059900                                    REG-UT-ART-VKART                      
060000     MOVE W46119-KDVSOP          TO RIS-KDVSOP                            
060100                                    REG-UT-ART-KDVSOP                     
060200                                                                          
060300     MOVE W46119-PRARTBTO-EXP    TO REG-UT-ART-PRARTBTO-EXP               
060400                                                                          
060500     MOVE W46119-IDSTATNR (01)   TO REG-UT-ART-IDSTATNR (01)              
060600     MOVE W46119-IDSTATNR (02)   TO REG-UT-ART-IDSTATNR (02)              
060700     MOVE W46119-IDSTATNR (03)   TO REG-UT-ART-IDSTATNR (03)              
060800     MOVE W46119-IDSTATNR (04)   TO REG-UT-ART-IDSTATNR (04)              
060900     MOVE W46119-IDSTATNR (05)   TO REG-UT-ART-IDSTATNR (05)              
061000     MOVE W46119-IDSTATNR (06)   TO REG-UT-ART-IDSTATNR (06)              
061100                                                                          
061200     MOVE W46119-KDUART          TO REG-UT-ART-KDUART                     
061300                                                                          
061400     MOVE NEJ                    TO RIS-FLMILART                          
061500                                                                          
061600     MOVE W46119-KDSORT          TO RIS-KDSORT                            
061700                                    REG-UT-ART-KDSORT                     
061800     MOVE W46119-KDERS           TO RIS-KDERS                             
061900                                    REG-UT-ART-KDERS                      
062000     MOVE W46119-KDBPSR          TO RIS-KDBPSR                            
062100                                    REG-UT-ART-KDBPSR                     
062200     MOVE W46119-IDLEVNR         TO RIS-IDLEVNR                           
062300                                    REG-UT-ART-IDLEVNR                    
062400     MOVE W46119-KDBBCL          TO RIS-KDBBCL                            
062500                                    REG-UT-ART-KDBBCL                     
062600     MOVE W46119-KDAGE           TO RIS-KDAGE                             
062700                                    REG-UT-ART-KDAGE                      
062800     MOVE W46119-KDARTRAB-A      TO REG-UT-ART-KDARTRAB-A                 
062900     MOVE W46119-KDARTRAB-B      TO REG-UT-ART-KDARTRAB-B                 
063000     MOVE W46119-KDARTRAB-C      TO REG-UT-ART-KDARTRAB-C                 
063100     MOVE W46119-KDARTRAB-D      TO REG-UT-ART-KDARTRAB-D                 
063200     MOVE W46119-KDARTRAB-E      TO REG-UT-ART-KDARTRAB-E                 
063300     MOVE W46119-KDARTRAB-F      TO REG-UT-ART-KDARTRAB-F                 
063400     MOVE W46119-KDARTRAB-G      TO REG-UT-ART-KDARTRAB-G                 
063500     MOVE W46119-PRARTBTO-MARK-A TO REG-UT-ART-PRARTBTO-MARK-A            
063600     MOVE W46119-PRARTBTO-MARK-B TO REG-UT-ART-PRARTBTO-MARK-B            
063700     MOVE W46119-PRARTBTO-MARK-C TO REG-UT-ART-PRARTBTO-MARK-C            
063800     MOVE W46119-PRARTBTO-MARK-D TO REG-UT-ART-PRARTBTO-MARK-D            
063900     MOVE W46119-PRARTBTO-MARK-E TO REG-UT-ART-PRARTBTO-MARK-E            
064000     MOVE W46119-PRARTBTO-MARK-F TO REG-UT-ART-PRARTBTO-MARK-F            
064100     MOVE W46119-PRARTBTO-MARK-G TO REG-UT-ART-PRARTBTO-MARK-G            
064200                                                                          
064300     MOVE 'RKA'                  TO RKA-IDPTYP                            
064400     .                                                                    
064500                                                                          
064600     EJECT                                                                
064700 CA-AENDRA-KDARTURS-TILL-NUM SECTION.                                     
064800     SKIP2                                                                
064900     MOVE W46119-KDARTURS        TO ARTU-KDARTURS                         
065000     MOVE ZERO                   TO ARTU-IDDISTR                          
065100     MOVE SPACE                  TO ARTU-IDDC                             
065200     CALL W400ARTU USING ARTU-W400ARTU                                    
065300     .                                                                    
065400     EJECT                                                                
065500                                                                          
065600 D-JAMFOR-FILER SECTION.                                                  
065700     SKIP2                                                                
065800******************************************************************        
065900*                                                                         
066000*    JÄMFÖR REG-UT OCH W46119 POSTER MED SAMMA ARTIKELNUMMER              
066100*    OM POSTERNA ÄR OLIKA SKRIVS POSTEN PÅ BÅDE W46161 OCH REG-UT         
066200*    ANNARS BARA PÅ REG-UT                                                
066300*                                                                         
066400******************************************************************        
066500*                                                                         
066600     IF W46119-REKSIFFR          NOT = REG-IN-ART-REKSIFFR                
066700     OR W46119-IDFKNGRP          NOT = REG-IN-ART-IDFKNGRP                
066800     OR W46119-KDPRODSL          NOT = REG-IN-ART-KDPRODSL                
066900     OR W46119-KDSRA             NOT = REG-IN-ART-KDSRA                   
067000     OR W46119-KVQPACK-1         NOT = REG-IN-ART-KVQPACK-1               
067100     OR W46119-KDARTURS          NOT = REG-IN-ART-KDARTURS                
067200     OR W46119-VLARTNTO          NOT = REG-IN-ART-VLARTNTO                
067300     OR W46119-VKART             NOT = REG-IN-ART-VKART                   
067400     OR W46119-KDVSOP            NOT = REG-IN-ART-KDVSOP                  
067500     OR W46119-KDBPSR            NOT = REG-IN-ART-KDBPSR                  
067600     OR W46119-PRARTBTO-EXP      NOT = REG-IN-ART-PRARTBTO-EXP            
067700     OR W46119-KDBBCL            NOT = REG-IN-ART-KDBBCL                  
067800     OR W46119-IDLEVNR           NOT = REG-IN-ART-IDLEVNR                 
067900     OR W46119-KDARTRAB-A        NOT = REG-IN-ART-KDARTRAB-A              
068000*    OR W46119-KDARTRAB-B        NOT = REG-IN-ART-KDARTRAB-B              
068100*    OR W46119-KDARTRAB-C        NOT = REG-IN-ART-KDARTRAB-C              
068200*    OR W46119-KDARTRAB-D        NOT = REG-IN-ART-KDARTRAB-D              
068300*    OR W46119-KDARTRAB-E        NOT = REG-IN-ART-KDARTRAB-E              
068400*    OR W46119-KDARTRAB-F        NOT = REG-IN-ART-KDARTRAB-F              
068500*    OR W46119-KDARTRAB-G        NOT = REG-IN-ART-KDARTRAB-G              
068600     OR W46119-PRARTBTO-MARK-A   NOT = REG-IN-ART-PRARTBTO-MARK-A         
068700*    OR W46119-PRARTBTO-MARK-B   NOT = REG-IN-ART-PRARTBTO-MARK-B         
068800*    OR W46119-PRARTBTO-MARK-C   NOT = REG-IN-ART-PRARTBTO-MARK-C         
068900*    OR W46119-PRARTBTO-MARK-D   NOT = REG-IN-ART-PRARTBTO-MARK-D         
069000*    OR W46119-PRARTBTO-MARK-E   NOT = REG-IN-ART-PRARTBTO-MARK-E         
069100*    OR W46119-PRARTBTO-MARK-F   NOT = REG-IN-ART-PRARTBTO-MARK-F         
069200*    OR W46119-PRARTBTO-MARK-G   NOT = REG-IN-ART-PRARTBTO-MARK-G         
069300     OR W46119-KDAGE             NOT = REG-IN-ART-KDAGE                   
069400       MOVE JA TO ART-ANDRAD                                              
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 S01-LAS-W46119 SECTION.                                                  
069900     SKIP2                                                                
070000     READ W46119 INTO W46119-AREA                                         
070100                      AT END MOVE JA TO W46119-EOF                        
070200                      MOVE 999999999 TO W46119-IDARTNR                    
070300     END-READ                                                             
070400     IF W46119-EOF = NEJ                                                  
070500       MOVE 'W46119'             TO POSTSUM-FDNAMN                        
070600       MOVE 'W46160D1'           TO POSTSUM-DDNAMN2                       
070700       MOVE ZERO                 TO POSTSUM-TRANSTYP                      
070800       CALL POSTSUM   USING POSTSUM-PARM                                  
070900     END-IF                                                               
071000     .                                                                    
071100     EJECT                                                                
071200 S02-LAS-REG-IN SECTION.                                                  
071300     SKIP2                                                                
071400     READ REG-IN INTO REG-IN-AREA                                         
071500                      AT END MOVE JA TO REG-IN-EOF                        
071600                      MOVE 999999999 TO REG-IN-ART-IDARTNR                
071700     END-READ                                                             
071800     IF REG-IN-EOF = NEJ                                                  
071900       MOVE 'REG-IN'             TO POSTSUM-FDNAMN                        
072000       MOVE 'W46160D2'          TO POSTSUM-DDNAMN2                        
072100       MOVE REG-IN-ART-IDPTYP TO POSTSUM-TRANSTYP                         
072200       CALL POSTSUM   USING POSTSUM-PARM                                  
072300     END-IF                                                               
072400     .                                                                    
072500     EJECT                                                                
072600 S10-SKRIV-REG-UT SECTION.                                                
072700     SKIP2                                                                
072800     WRITE REG-UT-POST FROM REG-UT-AREA                                   
072900     MOVE 'REG-UT'               TO POSTSUM-FDNAMN                        
073000     MOVE 'W46160DI'             TO POSTSUM-DDNAMN2                       
073100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
073200     CALL POSTSUM      USING POSTSUM-PARM                                 
073300     .                                                                    
073400     EJECT                                                                
073500 S11-SKRIV-TILL-IMPORTOR SECTION.                                         
073600     SKIP2                                                                
073700*                                                                         
073800*          **SVERIGE **                                                   
073900     MOVE +5 TO IX-STATNR                                                 
074000     MOVE +0 TO KDHBLKRV                                                  
074100     MOVE 'A' TO W-MARKNAD                                                
074200     PERFORM S11A-KOMPLETTERA                                             
074300     PERFORM S11B-SVE-ENGEL-BEART                                         
074400     PERFORM S11C-SE-SKRIV-W4616T                                         
074500                                                                          
074600     MOVE W46119-KDPRODSL        TO TEST-KDPRODSL                         
074700     IF KDPRODSL-BIMA                                                     
074800*********BIMA*******                                                      
074900       CONTINUE                                                           
075000     ELSE                                                                 
075100*            **NORGE **                                                   
075200       MOVE +2 TO IX-STATNR                                               
075300       MOVE +3 TO KDHBLKRV                                                
075400       MOVE 'A' TO W-MARKNAD                                              
075500       PERFORM S11A-KOMPLETTERA                                           
075600       PERFORM S11B-SVE-ENGEL-BEART                                       
075700       PERFORM S11C-NO-SKRIV-W4616V                                       
075800                                                                          
075900*            **DANMARK **                                                 
076000       MOVE +3 TO IX-STATNR                                               
076100       MOVE +1 TO KDHBLKRV                                                
076200       MOVE 'A' TO W-MARKNAD                                              
076300       PERFORM S11A-KOMPLETTERA                                           
076400       PERFORM S11B-SVE-ENGEL-BEART                                       
076500       PERFORM S11C-DK-SKRIV-W4616U                                       
076600     END-IF                                                               
076700                                                                          
076800     MOVE W46119-KDPRODSL        TO TEST-KDPRODSL                         
076900     IF KDPRODSL-BIMA                                                     
077000**** DESSA ARTIKLAR SÄNDS EJ TILL FÖLJANDE LÄNDER                         
077100       CONTINUE                                                           
077200     ELSE                                                                 
077300*           **FRANKRIKE **                                                
077400       MOVE +3 TO IX-STATNR                                               
077500       MOVE +1 TO KDHBLKRV                                                
077600       MOVE 'B' TO W-MARKNAD                                              
077700       PERFORM S11A-KOMPLETTERA                                           
077800       PERFORM S11B-FRA-ENGEL-BEART                                       
077900       PERFORM S11C-FR-SKRIV-W4617A                                       
078000*           **SCHWEIZ  DISTR 2070**                                       
078100       MOVE +3 TO IX-STATNR                                               
078200       MOVE +1 TO KDHBLKRV                                                
078300       MOVE 'B' TO W-MARKNAD                                              
078400       PERFORM S11A-KOMPLETTERA                                           
078500       PERFORM S11B-TYS-FRANS-BEART                                       
078600       PERFORM S11C-CH-SKRIV-W4616W                                       
078700*           **ITALIEN  ***                                                
078800       MOVE +3 TO IX-STATNR                                               
078900       MOVE +1 TO KDHBLKRV                                                
079000       MOVE 'B' TO W-MARKNAD                                              
079100       PERFORM S11A-KOMPLETTERA                                           
079200       PERFORM S11B-ITA-ENGEL-BEART                                       
079300       PERFORM S11C-IT-SKRIV-W4616G                                       
079400*           **FINLAND DISTR 1090   ***                                    
079500       MOVE +5 TO IX-STATNR                                               
079600       MOVE +3 TO KDHBLKRV                                                
079700       MOVE 'A' TO W-MARKNAD                                              
079800       PERFORM S11A-KOMPLETTERA                                           
079900       PERFORM S11B-FIN-SVENS-BEART                                       
080000       PERFORM S11C-FI-SKRIV-W4616I                                       
080100*           ***BELGIEN***                                                 
080200       MOVE +3 TO IX-STATNR                                               
080300       MOVE +1 TO KDHBLKRV                                                
080400       MOVE 'B' TO W-MARKNAD                                              
080500       PERFORM S11A-KOMPLETTERA                                           
080600       PERFORM S11B-FRA-HOLLA-BEART                                       
080700       PERFORM S11C-BE-SKRIV-W4616J                                       
080800*           ***AUSTRALIEN   **                                            
080900       MOVE +5 TO IX-STATNR                                               
081000       MOVE +1 TO KDHBLKRV                                                
081100       MOVE 'F' TO W-MARKNAD                                              
081200       PERFORM S11A-KOMPLETTERA                                           
081300       PERFORM S11B-ENG-SPACE-BEART                                       
081400       PERFORM S11C-AU-SKRIV-W4616K                                       
081500       EJECT                                                              
081600*           ***HOLLAND **                                                 
081700       MOVE +3 TO IX-STATNR                                               
081800       MOVE +1 TO KDHBLKRV                                                
081900       MOVE 'B' TO W-MARKNAD                                              
082000       PERFORM S11A-KOMPLETTERA                                           
082100       PERFORM S11B-HOL-ENGEL-BEART                                       
082200       PERFORM S11C-NL-SKRIV-W4616S                                       
082300*           ***TYSKLAND **                                                
082400       MOVE +3 TO IX-STATNR                                               
082500       MOVE +1 TO KDHBLKRV                                                
082600       MOVE 'B' TO W-MARKNAD                                              
082700       PERFORM S11A-KOMPLETTERA                                           
082800       PERFORM S11B-TYS-TYSKA-BEART                                       
082900       PERFORM S11C-DE-SKRIV-W46162                                       
083000*           ***PORTUGAL***                                                
083100       MOVE +3 TO IX-STATNR                                               
083200       MOVE +3 TO KDHBLKRV                                                
083300       MOVE 'B' TO W-MARKNAD                                              
083400       PERFORM S11A-KOMPLETTERA                                           
083500       PERFORM S11B-POR-ENGEL-BEART                                       
083600       PERFORM S11C-PT-SKRIV-W4616N                                       
083700*           ***SPANIEN *                                                  
083800       MOVE +3 TO IX-STATNR                                               
083900       MOVE +3 TO KDHBLKRV                                                
084000       MOVE 'B' TO W-MARKNAD                                              
084100       PERFORM S11A-KOMPLETTERA                                           
084200       PERFORM S11B-SPA-ENGEL-BEART                                       
084300       PERFORM S11C-ES-SKRIV-W4617B                                       
084400*           ***JAPAN***                                                   
084500       MOVE +3 TO IX-STATNR                                               
084600       MOVE +3 TO KDHBLKRV                                                
084700       MOVE 'F' TO W-MARKNAD                                              
084800       PERFORM S11A-KOMPLETTERA                                           
084900       PERFORM S11B-ENG-SPACE-BEART                                       
085000       PERFORM S11C-JP-SKRIV-W4616P                                       
085100*           ***ENGLAND DISTR 1312***                                      
085200       MOVE +3 TO IX-STATNR                                               
085300       MOVE +3 TO KDHBLKRV                                                
085400       MOVE 'F' TO W-MARKNAD                                              
085500       PERFORM S11A-KOMPLETTERA                                           
085600       PERFORM S11B-ENG-SPACE-BEART                                       
085700       PERFORM S11C-GB-SKRIV-W4616Q                                       
085800*           ***ENGLAND NC DISTR 1378***                                   
085900       MOVE +3 TO IX-STATNR                                               
086000       MOVE +3 TO KDHBLKRV                                                
086100       MOVE 'F' TO W-MARKNAD                                              
086200       PERFORM S11A-KOMPLETTERA                                           
086300       PERFORM S11B-ENG-SPACE-BEART                                       
086400       PERFORM S11C-GBNC-SKRIV-W4618F                                     
086500*           *****USA*****                                                 
086600       MOVE +2 TO IX-STATNR                                               
086700       MOVE +1 TO KDHBLKRV                                                
086800       MOVE 'E' TO W-MARKNAD                                              
086900       PERFORM S11A-KOMPLETTERA                                           
087000       PERFORM S11B-AME-ENGEL-BEART                                       
087100       PERFORM S11C-US-SKRIV-W4616R                                       
087200*           *****ÖSTERRIKE   *****                                        
087300       MOVE +0 TO IX-STATNR                                               
087400       MOVE +1 TO KDHBLKRV                                                
087500       MOVE 'B' TO W-MARKNAD                                              
087600       PERFORM S11A-KOMPLETTERA                                           
087700       PERFORM S11B-TYS-ENGEL-BEART                                       
087800       PERFORM S11C-AT-SKRIV-W4617C                                       
087900*           *****BRASILIEN ****                                           
088000       MOVE +0 TO IX-STATNR                                               
088100       MOVE +0 TO KDHBLKRV                                                
088200       MOVE 'F' TO W-MARKNAD                                              
088300       PERFORM S11A-KOMPLETTERA                                           
088400       PERFORM S11B-ENG-SPANS-BEART                                       
088500       PERFORM S11C-BR-SKRIV-W4617H                                       
088600*           *****PERU*****                                                
088700       MOVE +4 TO IX-STATNR                                               
088800       MOVE +0 TO KDHBLKRV                                                
088900       MOVE 'F' TO W-MARKNAD                                              
089000       PERFORM S11A-KOMPLETTERA                                           
089100       PERFORM S11B-SPA-ENGEL-BEART                                       
089200       PERFORM S11C-PE-SKRIV-W4617G                                       
089300*           **SCHWEIZ  DISTR 2078**                                       
089400       MOVE +3 TO IX-STATNR                                               
089500       MOVE +1 TO KDHBLKRV                                                
089600       MOVE 'B' TO W-MARKNAD                                              
089700       PERFORM S11A-KOMPLETTERA                                           
089800       PERFORM S11B-TYS-FRANS-BEART                                       
089900       PERFORM S11C-CH-SKRIV-W4617J                                       
090000*           **AUSTRALIEN DISTR 7838**                                     
090100       MOVE +5 TO IX-STATNR                                               
090200       MOVE +1 TO KDHBLKRV                                                
090300       MOVE 'F' TO W-MARKNAD                                              
090400       PERFORM S11A-KOMPLETTERA                                           
090500       PERFORM S11B-ENG-SPACE-BEART                                       
090600       PERFORM S11C-AU-SKRIV-W4617L                                       
090700*           **TAIWAN DISTR 6221**                                         
090800       MOVE +3 TO IX-STATNR                                               
090900       MOVE +0 TO KDHBLKRV                                                
091000       MOVE 'F' TO W-MARKNAD                                              
091100       PERFORM S11A-KOMPLETTERA                                           
091200       PERFORM S11B-ENG-SPACE-BEART                                       
091300       PERFORM S11C-TW-SKRIV-W4617M                                       
091400*           **THAILAND DISTR 6228**                                       
091500       MOVE +3 TO IX-STATNR                                               
091600       MOVE +0 TO KDHBLKRV                                                
091700       MOVE 'F' TO W-MARKNAD                                              
091800       PERFORM S11A-KOMPLETTERA                                           
091900       PERFORM S11B-ENG-SPACE-BEART                                       
092000       PERFORM S11C-TH-SKRIV-W4617P                                       
092100*           **MALAYSIA DISTR 5610**                                       
092200       MOVE +3 TO IX-STATNR                                               
092300       MOVE +0 TO KDHBLKRV                                                
092400       MOVE 'F' TO W-MARKNAD                                              
092500       PERFORM S11A-KOMPLETTERA                                           
092600       PERFORM S11B-SPACE-ENG-BEART                                       
092700       PERFORM S11C-MY-SKRIV-W4617Q                                       
092800*           **SAUDI DISTR 4840 - 4843 **                                  
092900       MOVE +3 TO IX-STATNR                                               
093000       MOVE +0 TO KDHBLKRV                                                
093100       MOVE 'F' TO W-MARKNAD                                              
093200       PERFORM S11A-KOMPLETTERA                                           
093300       PERFORM S11B-ENG-SPACE-BEART                                       
093400       PERFORM S11C-SA-SKRIV-W4618E                                       
093500*           **POLEN DISTR 2870 2878 **                                    
093600       MOVE +3 TO IX-STATNR                                               
093700       MOVE +0 TO KDHBLKRV                                                
093800       MOVE 'A' TO W-MARKNAD                                              
093900       PERFORM S11A-KOMPLETTERA                                           
094000       PERFORM S11B-ENG-SPACE-BEART                                       
094100       PERFORM S11C-PL-SKRIV-W4618G                                       
094200*           **FINLAND DISTR 1091 **                                       
094300       MOVE +3 TO IX-STATNR                                               
094400       MOVE +0 TO KDHBLKRV                                                
094500       MOVE 'A' TO W-MARKNAD                                              
094600       PERFORM S11A-KOMPLETTERA                                           
094700       PERFORM S11B-ENG-SPACE-BEART                                       
094800       PERFORM S11C-FI-SKRIV-W4618A                                       
094900*           **CANADA  DISTR 7674 8751 **                                  
095000       MOVE +3 TO IX-STATNR                                               
095100       MOVE +0 TO KDHBLKRV                                                
095200       MOVE 'E' TO W-MARKNAD                                              
095300       PERFORM S11A-KOMPLETTERA                                           
095400       PERFORM S11B-ENG-SPACE-BEART                                       
095500       PERFORM S11C-CA-SKRIV-W4618H                                       
095600*           **RYSSLAND DISTR 2640 / 2699 / 2602 /2697                     
095700       MOVE +3 TO IX-STATNR                                               
095800       MOVE +0 TO KDHBLKRV                                                
095900       MOVE 'A' TO W-MARKNAD                                              
096000       PERFORM S11A-KOMPLETTERA                                           
096100       PERFORM S11B-ENG-SPACE-BEART                                       
096200       PERFORM S11C-RU-SKRIV-W4618I                                       
096300*           **KINA DISTR                                                  
096400       MOVE +3 TO IX-STATNR                                               
096500       MOVE +0 TO KDHBLKRV                                                
096600       MOVE 'F' TO W-MARKNAD                                              
096700       PERFORM S11A-KOMPLETTERA                                           
096800       PERFORM S11B-ENG-SPACE-BEART                                       
096900       PERFORM S11C-CN-SKRIV-W4618J                                       
097000*           **SYDAFRIKA DISTR                                             
097100       MOVE +3 TO IX-STATNR                                               
097200       MOVE +0 TO KDHBLKRV                                                
097300       MOVE 'B' TO W-MARKNAD                                              
097400       PERFORM S11A-KOMPLETTERA                                           
097500       PERFORM S11B-ENG-SPACE-BEART                                       
097600       PERFORM S11C-ZA-SKRIV-W4618K                                       
097700*           ***PORTUGAL***1958                                            
097800       MOVE +3 TO IX-STATNR                                               
097900       MOVE +3 TO KDHBLKRV                                                
098000       MOVE 'B' TO W-MARKNAD                                              
098100       PERFORM S11A-KOMPLETTERA                                           
098200       PERFORM S11B-POR-ENGEL-BEART                                       
098300       PERFORM S11C-PT-SKRIV-W4618L                                       
098400     END-IF                                                               
098500     .                                                                    
098600     EJECT                                                                
098700                                                                          
098800 S11A-KOMPLETTERA SECTION.                                                
098900     SKIP2                                                                
099000     IF IX-STATNR = 0                                                     
099100       MOVE 0 TO RIS-IDSTATNR                                             
099200     ELSE                                                                 
099300       MOVE W46119-IDSTATNR (IX-STATNR) TO RIS-IDSTATNR                   
099400     END-IF                                                               
099500     IF KDHBLKRV = 0                                                      
099600       MOVE 1 TO IX-RAD                                                   
099700     ELSE                                                                 
099800       EVALUATE TRUE                                                      
099900       WHEN KDHBLKRV = 1                                                  
100000         MOVE 2 TO IX-RAD                                                 
100100       WHEN KDHBLKRV = 2                                                  
100200         MOVE 3 TO IX-RAD                                                 
100300       WHEN KDHBLKRV = 3                                                  
100400         MOVE 4 TO IX-RAD                                                 
100500       END-EVALUATE                                                       
100600     END-IF                                                               
100700     IF W46119-KDSRA      = 0                                             
100800       MOVE 1 TO IX-KOLUMN                                                
100900     ELSE                                                                 
101000       EVALUATE TRUE                                                      
101100       WHEN W46119-KDSRA      = 1                                         
101200         MOVE 2 TO IX-KOLUMN                                              
101300       WHEN W46119-KDSRA      = 2                                         
101400         MOVE 3 TO IX-KOLUMN                                              
101500       WHEN W46119-KDSRA      = 3                                         
101600         MOVE 4 TO IX-KOLUMN                                              
101700       WHEN W46119-KDSRA      = 4                                         
101800         MOVE 5 TO IX-KOLUMN                                              
101900       WHEN W46119-KDSRA      = 5                                         
102000         MOVE 6 TO IX-KOLUMN                                              
102100       WHEN W46119-KDSRA      = 6                                         
102200         MOVE 7 TO IX-KOLUMN                                              
102300       WHEN W46119-KDSRA      > 6                                         
102400         MOVE 8 TO IX-KOLUMN                                              
102500       END-EVALUATE                                                       
102600     END-IF                                                               
102700     MOVE KDSRA-VARDE (IX-RAD, IX-KOLUMN) TO RIS-KDSRA                    
102800                                                                          
102900*--- LÄGG TILL "OR W-MARKNAD = 'X'"  FÖR VARJE NYTT MB                    
103000*--- SOM TAR TOTALANSVAR FÖR ALLA PRISER/ALLA PS /RS 950704               
103100     MOVE W46119-KDPRODSL        TO TEST-KDPRODSL                         
103200     IF KDPRODSL-VCBV                                                     
103300        OR W-MARKNAD = 'B'                                                
103400        OR W-MARKNAD = 'A'                                                
103500        OR W-MARKNAD = 'E'                                                
103600        OR W-MARKNAD = 'F'                                                
103700                                                                          
103800       IF W-MARKNAD = 'A'                                                 
103900         MOVE W46119-KDARTRAB-A      TO RKA-KDRABATT                      
104000         MOVE W46119-PRARTBTO-MARK-A TO RIS-PRARTBTO-EXP                  
104100       ELSE                                                               
104200         EVALUATE TRUE                                                    
104300         WHEN W-MARKNAD = 'B'                                             
104400           MOVE W46119-KDARTRAB-B      TO RKA-KDRABATT                    
104500           MOVE W46119-PRARTBTO-MARK-B TO RIS-PRARTBTO-EXP                
104600         WHEN W-MARKNAD = 'C'                                             
104700           MOVE W46119-KDARTRAB-C      TO RKA-KDRABATT                    
104800           MOVE W46119-PRARTBTO-MARK-C TO RIS-PRARTBTO-EXP                
104900         WHEN W-MARKNAD = 'D'                                             
105000           MOVE W46119-KDARTRAB-D      TO RKA-KDRABATT                    
105100           MOVE W46119-PRARTBTO-MARK-D TO RIS-PRARTBTO-EXP                
105200         WHEN W-MARKNAD = 'E'                                             
105300           MOVE W46119-KDARTRAB-E      TO RKA-KDRABATT                    
105400           MOVE W46119-PRARTBTO-MARK-E TO RIS-PRARTBTO-EXP                
105500         WHEN W-MARKNAD = 'F'                                             
105600           MOVE W46119-KDARTRAB-F      TO RKA-KDRABATT                    
105700           MOVE W46119-PRARTBTO-MARK-F TO RIS-PRARTBTO-EXP                
105800         WHEN W-MARKNAD = 'G'                                             
105900           MOVE W46119-KDARTRAB-G      TO RKA-KDRABATT                    
106000           MOVE W46119-PRARTBTO-MARK-G TO RIS-PRARTBTO-EXP                
106100         END-EVALUATE                                                     
106200       END-IF                                                             
106300     ELSE                                                                 
106400       MOVE ZERO                       TO RKA-KDRABATT                    
106500       MOVE W46119-PRARTBTO-EXP        TO RIS-PRARTBTO-EXP                
106600     END-IF                                                               
106700     .                                                                    
106800     EJECT                                                                
106900 S11B-ENG-SPACE-BEART SECTION.                                            
107000     MOVE SPRAK-ENGELSK-BEART        TO RKA-BEART (1)                     
107100     MOVE SPACE                      TO RKA-BEART (2)                     
107200     .                                                                    
107300     SKIP2                                                                
107400 S11B-ENG-SPANS-BEART SECTION.                                            
107500     MOVE SPRAK-ENGELSK-BEART        TO RKA-BEART (1)                     
107600     MOVE SPRAK-SPANSK-BEART         TO RKA-BEART (2)                     
107700     .                                                                    
107800     EJECT                                                                
107900 S11B-FRA-ENGEL-BEART SECTION.                                            
108000     MOVE SPRAK-FRANSK-BEART         TO RKA-BEART (1)                     
108100     MOVE SPRAK-ENGELSK-BEART        TO RKA-BEART (2)                     
108200     .                                                                    
108300     SKIP2                                                                
108400 S11B-FRA-HOLLA-BEART SECTION.                                            
108500     MOVE SPRAK-FRANSK-BEART         TO RKA-BEART (1)                     
108600     MOVE SPRAK-HOLLANDSK-BEART      TO RKA-BEART (2)                     
108700     .                                                                    
108800     SKIP2                                                                
108900 S11B-SVE-ENGEL-BEART SECTION.                                            
109000     MOVE SPRAK-SVENSK-BEART         TO RKA-BEART (1)                     
109100     MOVE SPRAK-ENGELSK-BEART        TO RKA-BEART (2)                     
109200     .                                                                    
109300     SKIP2                                                                
109400 S11B-TYS-FRANS-BEART SECTION.                                            
109500     MOVE SPRAK-TYSK-BEART           TO RKA-BEART (1)                     
109600     MOVE SPRAK-FRANSK-BEART         TO RKA-BEART (2)                     
109700     .                                                                    
109800     SKIP2                                                                
109900 S11B-TYS-TYSKA-BEART SECTION.                                            
110000     MOVE SPRAK-TYSK-BEART           TO RKA-BEART (1)                     
110100     MOVE SPRAK-TYSK-BEART           TO RKA-BEART (2)                     
110200     .                                                                    
110300     SKIP2                                                                
110400 S11B-TYS-ENGEL-BEART SECTION.                                            
110500     MOVE SPRAK-TYSK-BEART           TO RKA-BEART (1)                     
110600     MOVE SPRAK-ENGELSK-BEART        TO RKA-BEART (2)                     
110700     .                                                                    
110800     EJECT                                                                
110900 S11B-ITA-ENGEL-BEART SECTION.                                            
111000     MOVE SPRAK-ITALIENSK-BEART      TO RKA-BEART (1)                     
111100     MOVE SPRAK-ENGELSK-BEART        TO RKA-BEART (2)                     
111200     .                                                                    
111300     SKIP2                                                                
111400 S11B-FIN-SVENS-BEART SECTION.                                            
111500     MOVE SPRAK-FINSK-BEART          TO RKA-BEART (1)                     
111600     MOVE SPRAK-SVENSK-BEART         TO RKA-BEART (2)                     
111700     .                                                                    
111800     SKIP2                                                                
111900 S11B-HOL-ENGEL-BEART SECTION.                                            
112000     MOVE SPRAK-HOLLANDSK-BEART      TO RKA-BEART (1)                     
112100     MOVE SPRAK-ENGELSK-BEART        TO RKA-BEART (2)                     
112200     .                                                                    
112300     SKIP2                                                                
112400 S11B-POR-ENGEL-BEART SECTION.                                            
112500     MOVE SPRAK-PORTUGISISK-BEART    TO RKA-BEART (1)                     
112600     MOVE SPRAK-ENGELSK-BEART        TO RKA-BEART (2)                     
112700     .                                                                    
112800     SKIP2                                                                
112900 S11B-SPA-ENGEL-BEART SECTION.                                            
113000     MOVE SPRAK-SPANSK-BEART         TO RKA-BEART (1)                     
113100     MOVE SPRAK-ENGELSK-BEART        TO RKA-BEART (2)                     
113200     .                                                                    
113300     SKIP2                                                                
113400 S11B-AME-ENGEL-BEART SECTION.                                            
113500     MOVE SPRAK-AMERIKANSK-BEART     TO RKA-BEART (1)                     
113600     MOVE SPRAK-ENGELSK-BEART        TO RKA-BEART (2)                     
113700     .                                                                    
113800     SKIP2                                                                
113900 S11B-SPACE-ENG-BEART SECTION.                                            
114000     MOVE SPACE                      TO RKA-BEART (1)                     
114100     MOVE SPRAK-ENGELSK-BEART        TO RKA-BEART (2)                     
114200     .                                                                    
114300     EJECT                                                                
114400 S11C-IT-SKRIV-W4616G SECTION.                                            
114500     SKIP1                                                                
114600     WRITE ITALY-POST FROM RIS-AREA                                       
114700     WRITE ITALY-POST FROM RKA-AREA                                       
114800     MOVE 'W4616G'               TO POSTSUM-FDNAMN                        
114900     MOVE 'W46160D3'             TO POSTSUM-DDNAMN2                       
115000     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
115100     CALL POSTSUM      USING POSTSUM-PARM                                 
115200     .                                                                    
115300     SKIP2                                                                
115400 S11C-FI-SKRIV-W4616I SECTION.                                            
115500     SKIP1                                                                
115600     WRITE FINL-POST FROM RIS-AREA                                        
115700     WRITE FINL-POST FROM RKA-AREA                                        
115800     MOVE 'W4616I'               TO POSTSUM-FDNAMN                        
115900     MOVE 'W46160DB'             TO POSTSUM-DDNAMN2                       
116000     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
116100     CALL POSTSUM      USING POSTSUM-PARM                                 
116200     .                                                                    
116300     SKIP2                                                                
116400 S11C-BE-SKRIV-W4616J SECTION.                                            
116500     SKIP1                                                                
116600     WRITE BELG-POST FROM RIS-AREA                                        
116700     WRITE BELG-POST FROM RKA-AREA                                        
116800     MOVE 'W4616J'               TO POSTSUM-FDNAMN                        
116900     MOVE 'W46160DC'             TO POSTSUM-DDNAMN2                       
117000     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
117100     CALL POSTSUM      USING POSTSUM-PARM                                 
117200     .                                                                    
117300     SKIP2                                                                
117400 S11C-PT-SKRIV-W4616N SECTION.                                            
117500     SKIP1                                                                
117600     WRITE PORT-POST FROM RIS-AREA                                        
117700     WRITE PORT-POST FROM RKA-AREA                                        
117800     MOVE 'W4616N'               TO POSTSUM-FDNAMN                        
117900     MOVE 'W46160DG'             TO POSTSUM-DDNAMN2                       
118000     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
118100     CALL POSTSUM      USING POSTSUM-PARM                                 
118200     .                                                                    
118300     EJECT                                                                
118400 S11C-JP-SKRIV-W4616P SECTION.                                            
118500     SKIP1                                                                
118600     WRITE JAPAN-POST FROM RIS-AREA                                       
118700     WRITE JAPAN-POST FROM RKA-AREA                                       
118800     MOVE 'W4616P'               TO POSTSUM-FDNAMN                        
118900     MOVE 'W46160D8'             TO POSTSUM-DDNAMN2                       
119000     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
119100     CALL POSTSUM      USING POSTSUM-PARM                                 
119200     .                                                                    
119300     EJECT                                                                
119400 S11C-GB-SKRIV-W4616Q SECTION.                                            
119500     SKIP1                                                                
119600     WRITE ENGLAND-POST FROM RIS-AREA                                     
119700     WRITE ENGLAND-POST FROM RKA-AREA                                     
119800     MOVE 'W4616Q'               TO POSTSUM-FDNAMN                        
119900     MOVE 'W46160D9'             TO POSTSUM-DDNAMN2                       
120000     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
120100     CALL POSTSUM      USING POSTSUM-PARM                                 
120200     .                                                                    
120300     EJECT                                                                
120400 S11C-GBNC-SKRIV-W4618F SECTION.                                          
120500     SKIP1                                                                
120600     WRITE ENGLAND-NC-POST FROM RIS-AREA                                  
120700     WRITE ENGLAND-NC-POST FROM RKA-AREA                                  
120800     MOVE 'W4618F'               TO POSTSUM-FDNAMN                        
120900     MOVE 'W46160DU'             TO POSTSUM-DDNAMN2                       
121000     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
121100     CALL POSTSUM      USING POSTSUM-PARM                                 
121200     .                                                                    
121300     EJECT                                                                
121400 S11C-PL-SKRIV-W4618G SECTION.                                            
121500     SKIP1                                                                
121600     WRITE POLEN-POST      FROM RIS-AREA                                  
121700     WRITE POLEN-POST      FROM RKA-AREA                                  
121800     MOVE 'W4618G'               TO POSTSUM-FDNAMN                        
121900     MOVE 'W46160DV'             TO POSTSUM-DDNAMN2                       
122000     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
122100     CALL POSTSUM      USING POSTSUM-PARM                                 
122200     .                                                                    
122300     EJECT                                                                
122400 S11C-FI-SKRIV-W4618A SECTION.                                            
122500     SKIP1                                                                
122600     WRITE FI-1091-POST    FROM RIS-AREA                                  
122700     WRITE FI-1091-POST    FROM RKA-AREA                                  
122800     MOVE 'W4618A'               TO POSTSUM-FDNAMN                        
122900     MOVE 'W46160DW'             TO POSTSUM-DDNAMN2                       
123000     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
123100     CALL POSTSUM      USING POSTSUM-PARM                                 
123200     .                                                                    
123300     EJECT                                                                
123400 S11C-CA-SKRIV-W4618H SECTION.                                            
123500     SKIP1                                                                
123600     WRITE CAN-POST        FROM RIS-AREA                                  
123700     WRITE CAN-POST        FROM RKA-AREA                                  
123800     MOVE 'W4618H'               TO POSTSUM-FDNAMN                        
123900     MOVE 'W46160DX'             TO POSTSUM-DDNAMN2                       
124000     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
124100     CALL POSTSUM      USING POSTSUM-PARM                                 
124200     .                                                                    
124300     EJECT                                                                
124400 S11C-US-SKRIV-W4616R SECTION.                                            
124500     SKIP1                                                                
124600     WRITE USA-POST FROM RIS-AREA                                         
124700     WRITE USA-POST FROM RKA-AREA                                         
124800     MOVE 'W4616R'               TO POSTSUM-FDNAMN                        
124900     MOVE 'W46160DL'             TO POSTSUM-DDNAMN2                       
125000     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
125100     CALL POSTSUM      USING POSTSUM-PARM                                 
125200     .                                                                    
125300     EJECT                                                                
125400 S11C-SE-SKRIV-W4616T SECTION.                                            
125500     SKIP1                                                                
125600     ADD +1                      TO POST-RAKNARE                          
125700     WRITE SWEDEN-POST FROM RIS-AREA                                      
125800     WRITE SWEDEN-POST FROM RKA-AREA                                      
125900     MOVE 'W4616T'               TO POSTSUM-FDNAMN                        
126000     MOVE 'W46160DN'             TO POSTSUM-DDNAMN2                       
126100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
126200     CALL POSTSUM      USING POSTSUM-PARM                                 
126300     .                                                                    
126400     EJECT                                                                
126500 S11C-NO-SKRIV-W4616V SECTION.                                            
126600     SKIP1                                                                
126700     WRITE NORWAY-POST FROM RIS-AREA                                      
126800     WRITE NORWAY-POST FROM RKA-AREA                                      
126900     MOVE 'W4616V'               TO POSTSUM-FDNAMN                        
127000     MOVE 'W46160DP'             TO POSTSUM-DDNAMN2                       
127100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
127200     CALL POSTSUM      USING POSTSUM-PARM                                 
127300     .                                                                    
127400     EJECT                                                                
127500 S11C-DK-SKRIV-W4616U SECTION.                                            
127600     SKIP1                                                                
127700     WRITE DENMARK-POST FROM RIS-AREA                                     
127800     WRITE DENMARK-POST FROM RKA-AREA                                     
127900     MOVE 'W4616U'               TO POSTSUM-FDNAMN                        
128000     MOVE 'W46160DO'             TO POSTSUM-DDNAMN2                       
128100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
128200     CALL POSTSUM      USING POSTSUM-PARM                                 
128300     .                                                                    
128400     EJECT                                                                
128500 S11C-NL-SKRIV-W4616S SECTION.                                            
128600     SKIP1                                                                
128700     WRITE HOLLAND-POST FROM RIS-AREA                                     
128800     WRITE HOLLAND-POST FROM RKA-AREA                                     
128900     MOVE 'W4616S'               TO POSTSUM-FDNAMN                        
129000     MOVE 'W46160DM'             TO POSTSUM-DDNAMN2                       
129100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
129200     CALL POSTSUM      USING POSTSUM-PARM                                 
129300     .                                                                    
129400     EJECT                                                                
129500 S11C-CH-SKRIV-W4616W SECTION.                                            
129600     SKIP1                                                                
129700     WRITE SWISS-IMP-POST FROM RIS-AREA                                   
129800     WRITE SWISS-IMP-POST FROM RKA-AREA                                   
129900     MOVE 'W4616W'               TO POSTSUM-FDNAMN                        
130000     MOVE 'W46160DQ'             TO POSTSUM-DDNAMN2                       
130100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
130200     CALL POSTSUM      USING POSTSUM-PARM                                 
130300     .                                                                    
130400     EJECT                                                                
130500 S11C-AU-SKRIV-W4616K SECTION.                                            
130600     SKIP1                                                                
130700     WRITE AUST-7836-POST FROM RIS-AREA                                   
130800     WRITE AUST-7836-POST FROM RKA-AREA                                   
130900     MOVE 'W4616K'               TO POSTSUM-FDNAMN                        
131000     MOVE 'W46160DS'             TO POSTSUM-DDNAMN2                       
131100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
131200     CALL POSTSUM      USING POSTSUM-PARM                                 
131300     .                                                                    
131400     EJECT                                                                
131500 S11C-FR-SKRIV-W4617A SECTION.                                            
131600     SKIP1                                                                
131700     WRITE FRANCE-POST FROM RIS-AREA                                      
131800     WRITE FRANCE-POST FROM RKA-AREA                                      
131900     MOVE 'W4617A'               TO POSTSUM-FDNAMN                        
132000     MOVE 'W46160DU'             TO POSTSUM-DDNAMN2                       
132100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
132200     CALL POSTSUM      USING POSTSUM-PARM                                 
132300     .                                                                    
132400     EJECT                                                                
132500 S11C-DE-SKRIV-W46162 SECTION.                                            
132600     SKIP1                                                                
132700     WRITE GERMANY-POST FROM RIS-AREA                                     
132800     WRITE GERMANY-POST FROM RKA-AREA                                     
132900     MOVE 'W46162'               TO POSTSUM-FDNAMN                        
133000     MOVE 'W46160DT'             TO POSTSUM-DDNAMN2                       
133100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
133200     CALL POSTSUM      USING POSTSUM-PARM                                 
133300     .                                                                    
133400     EJECT                                                                
133500 S11C-ES-SKRIV-W4617B SECTION.                                            
133600     SKIP1                                                                
133700     WRITE SPAIN-POST FROM RIS-AREA                                       
133800     WRITE SPAIN-POST FROM RKA-AREA                                       
133900     MOVE 'W4617B'               TO POSTSUM-FDNAMN                        
134000     MOVE 'W46160DK'             TO POSTSUM-DDNAMN2                       
134100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
134200     CALL POSTSUM      USING POSTSUM-PARM                                 
134300     .                                                                    
134400     EJECT                                                                
134500 S11C-AT-SKRIV-W4617C SECTION.                                            
134600     SKIP1                                                                
134700     WRITE AUSTRIA-POST FROM RIS-AREA                                     
134800     WRITE AUSTRIA-POST FROM RKA-AREA                                     
134900     MOVE 'W4617C'               TO POSTSUM-FDNAMN                        
135000     MOVE 'W46160DW'             TO POSTSUM-DDNAMN2                       
135100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
135200     CALL POSTSUM      USING POSTSUM-PARM                                 
135300     .                                                                    
135400     EJECT                                                                
135500 S11C-BR-SKRIV-W4617H SECTION.                                            
135600     SKIP1                                                                
135700     WRITE BRAZIL-POST FROM RIS-AREA                                      
135800     WRITE BRAZIL-POST FROM RKA-AREA                                      
135900     MOVE 'W4617H'               TO POSTSUM-FDNAMN                        
136000     MOVE 'W46160DX'             TO POSTSUM-DDNAMN2                       
136100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
136200     CALL POSTSUM      USING POSTSUM-PARM                                 
136300     .                                                                    
136400     EJECT                                                                
136500 S11C-PE-SKRIV-W4617G SECTION.                                            
136600     SKIP1                                                                
136700     WRITE PERU-POST FROM RIS-AREA                                        
136800     WRITE PERU-POST FROM RKA-AREA                                        
136900     MOVE 'W4617G'               TO POSTSUM-FDNAMN                        
137000     MOVE 'W46160DZ'             TO POSTSUM-DDNAMN2                       
137100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
137200     CALL POSTSUM      USING POSTSUM-PARM                                 
137300     .                                                                    
137400     EJECT                                                                
137500 S11C-CH-SKRIV-W4617J SECTION.                                            
137600     SKIP1                                                                
137700     WRITE SWISS-NC-POST FROM RIS-AREA                                    
137800     WRITE SWISS-NC-POST FROM RKA-AREA                                    
137900     MOVE 'W4617J'               TO POSTSUM-FDNAMN                        
138000     MOVE 'W46160DR'             TO POSTSUM-DDNAMN2                       
138100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
138200     CALL POSTSUM      USING POSTSUM-PARM                                 
138300     .                                                                    
138400     EJECT                                                                
138500 S11C-AU-SKRIV-W4617L SECTION.                                            
138600     SKIP1                                                                
138700     WRITE AUST-7838-POST FROM RIS-AREA                                   
138800     WRITE AUST-7838-POST FROM RKA-AREA                                   
138900     MOVE 'W4617L'               TO POSTSUM-FDNAMN                        
139000     MOVE 'W46160DO'             TO POSTSUM-DDNAMN2                       
139100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
139200     CALL POSTSUM      USING POSTSUM-PARM                                 
139300     .                                                                    
139400     EJECT                                                                
139500 S11C-TW-SKRIV-W4617M SECTION.                                            
139600     SKIP1                                                                
139700     WRITE TAIWAN-POST FROM RIS-AREA                                      
139800     WRITE TAIWAN-POST FROM RKA-AREA                                      
139900     MOVE 'W4617M'               TO POSTSUM-FDNAMN                        
140000     MOVE 'W46160DP'             TO POSTSUM-DDNAMN2                       
140100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
140200     CALL POSTSUM      USING POSTSUM-PARM                                 
140300     .                                                                    
140400     EJECT                                                                
140500 S11C-TH-SKRIV-W4617P SECTION.                                            
140600     SKIP1                                                                
140700     WRITE THAILAND-POST FROM RIS-AREA                                    
140800     WRITE THAILAND-POST FROM RKA-AREA                                    
140900     MOVE 'W4617P'               TO POSTSUM-FDNAMN                        
141000     MOVE 'W46160DR'             TO POSTSUM-DDNAMN2                       
141100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
141200     CALL POSTSUM      USING POSTSUM-PARM                                 
141300     .                                                                    
141400     EJECT                                                                
141500 S11C-MY-SKRIV-W4617Q SECTION.                                            
141600     SKIP1                                                                
141700     WRITE MALAYSIA-POST FROM RIS-AREA                                    
141800     WRITE MALAYSIA-POST FROM RKA-AREA                                    
141900     MOVE 'W4617Q'               TO POSTSUM-FDNAMN                        
142000     MOVE 'W46160DS'             TO POSTSUM-DDNAMN2                       
142100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
142200     CALL POSTSUM      USING POSTSUM-PARM                                 
142300     .                                                                    
142400     EJECT                                                                
142500 S11C-SA-SKRIV-W4618E SECTION.                                            
142600     SKIP1                                                                
142700     WRITE SAUDI-POST FROM RIS-AREA                                       
142800     WRITE SAUDI-POST FROM RKA-AREA                                       
142900     MOVE 'W4618E'               TO POSTSUM-FDNAMN                        
143000     MOVE 'W46160DT'             TO POSTSUM-DDNAMN2                       
143100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
143200     CALL POSTSUM      USING POSTSUM-PARM                                 
143300     .                                                                    
143400     EJECT                                                                
143500 S11C-RU-SKRIV-W4618I SECTION.                                            
143600     SKIP1                                                                
143700     WRITE RYSS-POST  FROM RIS-AREA                                       
143800     WRITE RYSS-POST  FROM RKA-AREA                                       
143900     MOVE 'W4618I'               TO POSTSUM-FDNAMN                        
144000     MOVE 'W46160DY'             TO POSTSUM-DDNAMN2                       
144100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
144200     CALL POSTSUM      USING POSTSUM-PARM                                 
144300     .                                                                    
144400     EJECT                                                                
144500 S11C-CN-SKRIV-W4618J SECTION.                                            
144600     SKIP1                                                                
144700     WRITE KINA-POST  FROM RIS-AREA                                       
144800     WRITE KINA-POST  FROM RKA-AREA                                       
144900     MOVE 'W4618J'              TO POSTSUM-FDNAMN                         
145000     MOVE 'W46160DZ'             TO POSTSUM-DDNAMN2                       
145100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
145200     CALL POSTSUM      USING POSTSUM-PARM                                 
145300     .                                                                    
145400     EJECT                                                                
145500 S11C-ZA-SKRIV-W4618K SECTION.                                            
145600     SKIP1                                                                
145700     WRITE SYDAF-POST  FROM RIS-AREA                                      
145800     WRITE SYDAF-POST  FROM RKA-AREA                                      
145900     MOVE 'W4618K'               TO POSTSUM-FDNAMN                        
146000     MOVE 'W46160EA'             TO POSTSUM-DDNAMN2                       
146100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
146200     CALL POSTSUM      USING POSTSUM-PARM                                 
146300     .                                                                    
146400     EJECT                                                                
146500 S11C-PT-SKRIV-W4618L SECTION.                                            
146600     SKIP1                                                                
146700     WRITE PORTU-POST  FROM RIS-AREA                                      
146800     WRITE PORTU-POST  FROM RKA-AREA                                      
146900     MOVE 'W4618L'               TO POSTSUM-FDNAMN                        
147000     MOVE 'W46160EB'             TO POSTSUM-DDNAMN2                       
147100     MOVE RIS-IDPTYP             TO POSTSUM-TRANSTYP                      
147200     CALL POSTSUM      USING POSTSUM-PARM                                 
147300     .                                                                    
147400     EJECT                                                                
147500 S12-HAMTA-BENAMNING SECTION.                                             
147600                                                                          
147700     MOVE SPACE TO SPRAK-AREA                                             
147800     MOVE +1 TO IX                                                        
147900                                                                          
148000     PERFORM UNTIL IX > 10                                                
148100       IF W46119-IDSKYLT (IX) = 'D  '                                     
148200         MOVE W46119-BEART (IX)  TO SPRAK-TYSK-BEART                      
148300       ELSE                                                               
148400         EVALUATE TRUE                                                    
148500         WHEN W46119-IDSKYLT (IX) = 'E  '                                 
148600           MOVE W46119-BEART (IX)  TO SPRAK-SPANSK-BEART                  
148700         WHEN W46119-IDSKYLT (IX) = 'F  '                                 
148800           MOVE W46119-BEART (IX)  TO SPRAK-FRANSK-BEART                  
148900         WHEN W46119-IDSKYLT (IX) = 'GB '                                 
149000           MOVE W46119-BEART (IX)  TO SPRAK-ENGELSK-BEART                 
149100         WHEN W46119-IDSKYLT (IX) = 'I  '                                 
149200           MOVE W46119-BEART (IX)  TO SPRAK-ITALIENSK-BEART               
149300         WHEN W46119-IDSKYLT (IX) = 'NL '                                 
149400           MOVE W46119-BEART (IX)  TO SPRAK-HOLLANDSK-BEART               
149500         WHEN W46119-IDSKYLT (IX) = 'P  '                                 
149600           MOVE W46119-BEART (IX)  TO SPRAK-PORTUGISISK-BEART             
149700         WHEN W46119-IDSKYLT (IX) = 'S  '                                 
149800           MOVE W46119-BEART (IX)  TO SPRAK-SVENSK-BEART                  
149900         WHEN W46119-IDSKYLT (IX) = 'SF '                                 
150000           MOVE W46119-BEART (IX)  TO SPRAK-FINSK-BEART                   
150100         WHEN W46119-IDSKYLT (IX) = 'USA'                                 
150200           MOVE W46119-BEART (IX)  TO SPRAK-AMERIKANSK-BEART              
150300         END-EVALUATE                                                     
150400       END-IF                                                             
150500       ADD +1 TO IX                                                       
150600     END-PERFORM                                                          
150700     .                                                                    
150800     EJECT                                                                
150900 Z-FINIT SECTION.                                                         
151000     SKIP2                                                                
151100     CLOSE REG-IN                                                         
151200           W46119                                                         
151300           REG-UT                                                         
151400           W4616G                                                         
151500           W4616I                                                         
151600           W4616J                                                         
151700           W4616N                                                         
151800           W4616P                                                         
151900           W4616Q                                                         
152000           W4616R                                                         
152100           W4616S                                                         
152200           W4616T                                                         
152300           W4616U                                                         
152400           W4616V                                                         
152500           W4616W                                                         
152600           W4617J                                                         
152700           W4616K                                                         
152800           W46162                                                         
152900           W4617A                                                         
153000           W4617B                                                         
153100           W4617C                                                         
153200           W4617H                                                         
153300           W4617G                                                         
153400           W4617L                                                         
153500           W4617M                                                         
153600           W4617P                                                         
153700           W4617Q                                                         
153800           W4618E                                                         
153900           W4618F                                                         
154000           W4618G                                                         
154100           W4618A                                                         
154200           W4618H                                                         
154300           W4618I                                                         
154400           W4618J                                                         
154500           W4618K                                                         
154600           W4618L                                                         
154700     SKIP2                                                                
154800     MOVE 'S' TO POSTSUM-OPKOD                                            
154900     CALL POSTSUM USING POSTSUM-PARM                                      
155000                                                                          
155100     IF POST-RAKNARE > POST-MAX                                           
155200       OPEN OUTPUT W4616G                                                 
155300                   W4616I                                                 
155400                   W4616J                                                 
155500                   W4616N                                                 
155600                   W4616P                                                 
155700                   W4616Q                                                 
155800                   W4616R                                                 
155900                   W4616S                                                 
156000                   W4616T                                                 
156100                   W4616U                                                 
156200                   W4616V                                                 
156300                   W4616W                                                 
156400                   W4617J                                                 
156500                   W4616K                                                 
156600                   W46162                                                 
156700                   W4617A                                                 
156800                   W4617B                                                 
156900                   W4617C                                                 
157000                   W4617H                                                 
157100                   W4617G                                                 
157200                   W4617L                                                 
157300                   W4617M                                                 
157400                   W4617P                                                 
157500                   W4617Q                                                 
157600                   W4618E                                                 
157700                   W4618F                                                 
157800                   W4618G                                                 
157900                   W4618A                                                 
158000                   W4618H                                                 
158100                   W4618I                                                 
158200                   W4618J                                                 
158300                   W4618K                                                 
158400                   W4618L                                                 
158500                                                                          
158600       CLOSE       W4616G                                                 
158700                   W4616I                                                 
158800                   W4616J                                                 
158900                   W4616N                                                 
159000                   W4616P                                                 
159100                   W4616Q                                                 
159200                   W4616R                                                 
159300                   W4616S                                                 
159400                   W4616T                                                 
159500                   W4616U                                                 
159600                   W4616V                                                 
159700                   W4616W                                                 
159800                   W4617J                                                 
159900                   W4616K                                                 
160000                   W46162                                                 
160100                   W4617A                                                 
160200                   W4617B                                                 
160300                   W4617C                                                 
160400                   W4617H                                                 
160500                   W4617G                                                 
160600                   W4617L                                                 
160700                   W4617M                                                 
160800                   W4617P                                                 
160900                   W4617Q                                                 
161000                   W4618E                                                 
161100                   W4618F                                                 
161200                   W4618G                                                 
161300                   W4618A                                                 
161400                   W4618H                                                 
161500                   W4618I                                                 
161600                   W4618J                                                 
161700                   W4618K                                                 
161800                   W4618L                                                 
161900     END-IF                                                               
162000     .                                                                    
162100     EJECT                                                                
162200     EJECT                                                                
162300*    -COPY WY2000P9                                                       
