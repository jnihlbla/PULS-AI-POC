001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W3717000.                                                
001200 AUTHOR.         BO HAMMARIN.                                             
001300 DATE-WRITTEN.   00/04/05.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        PGM LÄSER SEGMENT WDA901/WDA912/WDA921 OCH                       
001710*        SELEKTERAR/SKRIVER EN FIL                                        
001800*                                                                         
001910*        PROGRAMMET LÄSER      WDA9                                       
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003202*          --- SELEKTERADE PERIODTRANSAKTIONER                            
003210     SELECT W37170                     ASSIGN TO W37170D1.                
003400     EJECT                                                                
003410                                                                          
003500 DATA DIVISION.                                                           
003600                                                                          
003700 FILE SECTION.                                                            
003802 FD  W37170                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  POST -COPY W37170 -PRE  UT-  -L.                                     
003900     EJECT                                                                
003910                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W3717000'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004700     EJECT                                                                
004710                                                                          
004800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES DAGENS-DATUM.                                       
005000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005201                                                                          
005210 01  WS-DAAAPP-LIMIT             PIC 9(6)    VALUE 200000.                
005220 01  WS-DAAAPP                   PIC 9(6)    VALUE 200000.                
005300     EJECT                                                                
005310                                                                          
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006001     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006010     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006100                                                                          
006200*    --- PARAMETRAR TILL ABEND                                            
006300     EJECT                                                                
006310                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700                                                                          
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007100     EJECT                                                                
007110                                                                          
007200*    --- PARAMETRAR TILL DATKORT                                          
007300*                                                                         
007400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W37170'.              
007500                                                                          
007600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007700                                                                          
007800*01  -COPY WDATKORT                                                       
007901     EJECT                                                                
007902                                                                          
007903*    --- PARAMETRAR TILL POSTSUM                                          
007904*                                                                         
007910*01  -COPY W0005   -PRE  POSTSUM-                                         
008001     EJECT                                                                
008002                                                                          
008010*01  -COPY WDATAREA                                                       
008101     EJECT                                                                
008102                                                                          
008103 01  UT-AREA-START               PIC X(24)   VALUE                        
008104                                 'UT-AREA-START  '.                       
008106                                                                          
008110*01  AREA -COPY W37170     -PRE UT-                                       
008200     EJECT                                                                
008210                                                                          
008300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008400*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800 01  NYCKLAR-TILL-DLI.                                                    
008901     03  W-IDARTNR-X.                                                     
008902         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008903     03  W-IDDISTR-X.                                                     
008904         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
008905     03  W-DAAAPP-X.                                                      
008910         05  W-DAAAPP            PIC S9(6)   VALUE ZERO COMP-3.           
009000                                                                          
009100*    --- STATUS-KOD FRÅN IMS                                              
009200 01  STATUS-WS                   PIC XX.                                  
009300     88  SEGMENT-FINNS                       VALUE '  '.                  
009400     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
009500                                                                          
009600 01  GODK-STATUSKODER.                                                    
009700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800     SKIP3                                                                
009900 01  SSA1                        PIC X(64).                               
010000 01  SSA2                        PIC X(64).                               
010100     EJECT                                                                
010110                                                                          
010200*    --- IMS FUNKTIONSKODER                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
010500                                                                          
010600*    ---  DLI INPUT-OUTPUT AREA                                           
010701 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA  '.                      
010702 01  DLI-IO-AREA.                                                         
010703   03  IO-AREA                   PIC X(100).                              
010704*  03  WDA901 -COPY WDA901                 -RED IO-AREA.                  
010706     EJECT                                                                
010707                                                                          
010708*  03  WDA912 -COPY WDA912                 -RED IO-AREA.                  
011000     EJECT                                                                
011010                                                                          
011020*  03  WDA921 -COPY WDA921                 -RED IO-AREA.                  
011030     EJECT                                                                
011040                                                                          
011100 LINKAGE SECTION.                                                         
011302*01  -COPY W0008  -PRE WDA9-                                              
011310     05  FILLER                  PIC X.                                   
011400     EJECT                                                                
011500                                                                          
011501 PROCEDURE DIVISION  USING WDA9-PCB.                                      
011502 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING WDA9-PCB.                                      
011800                                                                          
011900     PERFORM A-INIT                                                       
012000                                                                          
012101     PERFORM IMS-GET-WDA9                                                 
012102     PERFORM UNTIL SEGMENT-SAKNAS                                         
012103       EVALUATE WDA9-SEG-NAME-FB                                          
012108         WHEN 'WDA901'                                                    
012109           MOVE UPB-IDARTNR   TO UT-IDARTNR                               
012110         WHEN 'WDA912'                                                    
012111           PERFORM B-KONTROLL-912                                         
012112         WHEN 'WDA921'                                                    
012113           PERFORM C-KONTROLL-921                                         
012114       END-EVALUATE                                                       
012115       PERFORM IMS-GET-WDA9                                               
012120     END-PERFORM                                                          
012130                                                                          
012200     PERFORM Z-FINIT                                                      
012300                                                                          
012400     MOVE ZERO TO RETURN-CODE                                             
012500     GOBACK                                                               
012600     .                                                                    
012700     EJECT                                                                
012710                                                                          
012800 A-INIT SECTION.                                                          
013010     OPEN OUTPUT W37170                                                   
013100                                                                          
013200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
013300     MOVE D-AAR                         TO DAGENS-DATUM-AAR               
013400     MOVE D-MAANAD                      TO DAGENS-DATUM-MAANAD            
013500     MOVE D-DAG                         TO DAGENS-DATUM-DAG               
013610     MOVE IDPGM                         TO POSTSUM-PROGNAMN               
013611                                                                          
013620     MOVE 'AAMMDD'                      TO DAT-KDDATFORM                  
013630     MOVE DAGENS-DATUM                  TO DAT-I-TIDATUM                  
013640                                                                          
013650     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
013660                         DAT-O-TIDATUM DAT-KDSVAR                         
013670                                                                          
013680     IF DAT-KDSVAR-OK                                                     
013690       ADD DAT-TIAAPP                   TO   WS-DAAAPP-LIMIT              
013691       SUBTRACT 100                     FROM WS-DAAAPP-LIMIT              
013700     ELSE                                                                 
013710       DISPLAY 'FELAKTIGT DATUM'                                          
013720       CALL FELLOG                                                        
013730     END-IF                                                               
013800     .                                                                    
013900     EJECT                                                                
013910                                                                          
014000 B-KONTROLL-912 SECTION.                                                  
014070     IF UPA-DAAAPP >= WS-DAAAPP-LIMIT                                     
014080       MOVE UPA-SUSKROT-DC  TO UT-SUSKROT                                 
014084       MOVE UPA-SUINVEST-DC TO UT-SUINVEST                                
014085       MOVE UPA-SULEVANT-DC TO UT-SULEVANT-DC                             
014086       MOVE UPA-SUMOTT-CP   TO UT-SUMOTT-CP                               
014087                                                                          
014090       PERFORM S11-SKRIV-W37170                                           
014093     END-IF                                                               
014300     .                                                                    
014501     EJECT                                                                
014502                                                                          
014503 C-KONTROLL-921 SECTION.                                                  
014504     IF UPP-DAAAPP >= WS-DAAAPP-LIMIT                                     
014505       COMPUTE UT-SUSKROT = UPP-SUSKROT1-REM +                            
014506                            UPP-SUSKROT2-REM                              
014508       END-COMPUTE                                                        
014509       MOVE UPP-SUINVEST-REM TO UT-SUINVEST                               
014510       MOVE ZERO             TO UT-SULEVANT-DC                            
014511                                UT-SUMOTT-CP                              
014512                                                                          
014514       PERFORM S11-SKRIV-W37170                                           
014515     END-IF                                                               
014516     .                                                                    
014517     EJECT                                                                
014518                                                                          
014519 S11-SKRIV-W37170 SECTION.                                                
014520     WRITE UT-POST FROM UT-AREA                                           
014521                                                                          
014522     MOVE 'UT-'      TO POSTSUM-TRANSTYP                                  
014523     MOVE 'W37170'   TO POSTSUM-FDNAMN                                    
014524     MOVE 'W37170D1' TO POSTSUM-DDNAMN2                                   
014525     CALL POSTSUM USING POSTSUM-PARM                                      
014530     .                                                                    
014700     EJECT                                                                
014710                                                                          
015320 Z-FINIT SECTION.                                                         
015330     CLOSE W37170                                                         
015340     SKIP2                                                                
015350     MOVE 'S' TO POSTSUM-OPKOD                                            
015360     CALL POSTSUM USING POSTSUM-PARM                                      
015370     .                                                                    
015380     EJECT                                                                
015390                                                                          
015400* --- IMS SEKTIONER ---                                                   
015500                                                                          
015602 IMS-GET-WDA9   SECTION.                                                  
015604     CALL CBLTDLI USING GN WDA9-PCB DLI-IO-AREA                           
015605     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
015606     MOVE '  GAGKGB'       TO GODK-STATUSKODER                            
015607     PERFORM IMS-STATUSKONTROLL                                           
015610     .                                                                    
015700     EJECT                                                                
015710                                                                          
015800 IMS-STATUSKONTROLL SECTION.                                              
016000     SET STATUS-IX TO 1                                                   
016100     SEARCH GODK-STATUS                                                   
016200       AT END                                                             
016300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016400           DELIMITED BY SIZE INTO FELTEXT                                 
016500         DISPLAY FELTEXT                                                  
016600         CALL FELLOG                                                      
016700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016800         CONTINUE                                                         
016900     END-SEARCH                                                           
017000     .                                                                    
