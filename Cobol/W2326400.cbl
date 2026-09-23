001000 ID DIVISION.                                                             
001100 PROGRAM-ID.     W2326400.                                                
001200 AUTHOR.         PER-ANDERS HELGEGREN.                                    
001300 DATE-WRITTEN.   97/06/13.                                                
001400 DATE-COMPILED.                                                           
001500                                                                          
001600*    FUNKTION:                                                            
001700*        NEDLÄSNING AV OI INNEVARANDE PERIOD                              
001800*                                                                         
001910*        PROGRAMMET LÄSER      WLOIGB (WDL8)                              
002000*                                                                         
002010*    OBS  DATUMKORT GER PERIOD (INNEVARANDE)                              
002020*         DATKONV GER STARTVECKA I PERIODEN                               
002030*         DATUMKORT GER INNEVARANDE VECKA = SLUTVECKA FÖR PERIODEN        
002040*                                                                         
002050*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- NEDLÄST OI DENNA PERIOD                                    
003210     SELECT W23265                     ASSIGN TO W23264D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  W23265                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003810*01  POST -COPY W23265 -PRE  UT-  -L.                                     
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W2326400'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 01  IX                          PIC S9(3)  COMP-3.                       
004600 01  KVV                         PIC S9(3)  COMP-3.                       
004610 01  KVV-MAX                     PIC S9(3)  COMP-3.                       
004620 01  WS-IDARTNR                  PIC S9(9)  COMP-3.                       
004700     SKIP3                                                                
004800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES DAGENS-DATUM.                                       
005000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005210                                                                          
005220 01  WS-TIAAPP                   PIC 9(4).                                
005230 01  FILLER REDEFINES WS-TIAAPP.                                          
005240     03  WS-AAR                  PIC 9(2).                                
005250     03  WS-PERIOD               PIC 9(2).                                
005260                                                                          
005261 01  WS-TIAAAAVV                 PIC 9(6).                                
005262 01  FILLER  REDEFINES WS-TIAAAAVV.                                       
005263     03  WS-TISEKEL              PIC 9(2).                                
005270     03  WS-TIAAVV               PIC 9(4).                                
005271     03  FILLER  REDEFINES WS-TIAAVV.                                     
005272         05  WS-TIAA             PIC 9(2).                                
005273         05  WS-TIVV             PIC 9(2).                                
005274 01  FILLER  REDEFINES WS-TIAAAAVV.                                       
005275     03  WS-TIAAAA               PIC 9(4).                                
005276     03  FILLER                  PIC 9(2).                                
005277                                                                          
005280 01  WS-KVVIPER                  PIC 9.                                   
005291 01  VADD-AAVV                   PIC S9(5) COMP-3.                        
005292 01  VADD-ANTAL                  PIC S9(3) COMP-3.                        
005293 01  WS-TOTAL                    PIC S9(9) COMP-3.                        
005294                                                                          
005295 01  START-AAVV                  PIC 9(4).                                
005296 01  FILLER REDEFINES START-AAVV.                                         
005297     03  START-AA                PIC 9(2).                                
005298     03  START-VV                PIC 9(2).                                
005299                                                                          
005300 01  SLUT-VV                     PIC 9(2).                                
005310     EJECT                                                                
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005610     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006001     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006010     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006020     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
006100     SKIP2                                                                
006200*    --- PARAMETRAR TILL ABEND                                            
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL DATKORT                                          
007300*                                                                         
007400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23264'.              
007500     SKIP2                                                                
007600 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007700     SKIP2                                                                
007800*01  -COPY WDATKORT                                                       
007901     EJECT                                                                
007902*    --- PARAMETRAR TILL POSTSUM                                          
007903*                                                                         
007910*01  -COPY W0005   -PRE  POSTSUM-                                         
008001     EJECT                                                                
008010*01  -COPY WDATAREA                                                       
008101     EJECT                                                                
008102 01  UT-AREA-START               PIC X(24)   VALUE                        
008103                                 'UT-AREA-START  '.                       
008105                                                                          
008110*01  AREA -COPY W23265     -PRE UT-                                       
008200     EJECT                                                                
008300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008400*                                                                         
008600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008700     SKIP3                                                                
008800 01  NYCKLAR-TILL-DLI.                                                    
008901     03  W-IDARTNR-X.                                                     
008902         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008903     03  W-TIAAAA-X.                                                      
008910         05  W-TIAAAA            PIC 9(4)    VALUE ZERO.                  
009000     SKIP2                                                                
009100*    --- STATUS-KOD FRÅN IMS                                              
009200 01  STATUS-WS                   PIC XX.                                  
009300     88  SEGMENT-FINNS                       VALUE '  '.                  
009400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009510     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009600     SKIP2                                                                
009700 01  GODK-STATUSKODER.                                                    
009800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009900     SKIP3                                                                
010000 01  SSA1                        PIC X(64).                               
010100 01  SSA2                        PIC X(64).                               
010200     EJECT                                                                
010300*    --- IMS FUNKTIONSKODER                                               
010400*01  -COPY W0003                                                          
010500     EJECT                                                                
010700*    ---  DLI INPUT-OUTPUT AREA                                           
010801 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA    '.                    
010802                                                                          
010803 01  DLI-IO-AREA         PIC X(2000).                                     
010804                                                                          
010805 01  DLI-IO-WLOIGB01  REDEFINES DLI-IO-AREA.                              
010806*    03  -COPY WDL801  -PRE OIGB-                                         
010807     EJECT                                                                
010809 01  DLI-IO-WLOIGB11  REDEFINES DLI-IO-AREA.                              
010810*    03  -COPY WDL811  -PRE OIGB-                                         
011100     EJECT                                                                
011110                                                                          
011200 LINKAGE SECTION.                                                         
011300                                                                          
011402*01  -COPY W0008  -PRE WDL8-                                              
011410     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011601 PROCEDURE DIVISION  USING WDL8-PCB.                                      
011602 MAIN SECTION.                                                            
011610     ENTRY 'DLITCBL' USING WDL8-PCB.                                      
011700                                                                          
011900                                                                          
012000     PERFORM A-INIT                                                       
012010                                                                          
012011     PERFORM IMS-GET-OIGB                                                 
012013                                                                          
012014     PERFORM UNTIL SEGMENT-SLUT                                           
012015       EVALUATE WDL8-SEG-NAME-FB                                          
012016         WHEN 'WDL801'                                                    
012017           MOVE OIGB-ART-IDARTNR TO WS-IDARTNR                            
012018         WHEN 'WDL811'                                                    
012019           IF WS-TIAAAA = OIGB-AAR-TIAAAA                                 
012020              PERFORM B-NOLLA-ADDERA                                      
012021              PERFORM S11-SKRIV-W23265                                    
012022           END-IF                                                         
012023       END-EVALUATE                                                       
012024       PERFORM IMS-GET-OIGB                                               
012025     END-PERFORM                                                          
012026                                                                          
013300                                                                          
013400     PERFORM Z-FINIT                                                      
013500                                                                          
013600     MOVE ZERO TO RETURN-CODE                                             
013700     GOBACK                                                               
013800     .                                                                    
013900     EJECT                                                                
014000 A-INIT SECTION.                                                          
014201                                                                          
014202     MOVE IDPGM       TO POSTSUM-PROGNAMN                                 
014203                                                                          
014210     OPEN OUTPUT W23265                                                   
014300                                                                          
014400     CALL DATKORT USING  PROGRAM-NAMN DATUMKORT-ID DATUMKORT              
014500     MOVE D-AAR       TO DAGENS-DATUM-AAR          WS-AAR                 
014600     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
014700     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
014720     MOVE D-VECKA     TO SLUT-VV                                          
014800                                                                          
014802     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
014803     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
014804                                                                          
014805     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
014806                     DAT-O-TIDATUM DAT-KDSVAR                             
014807                                                                          
014808     IF DAT-KDSVAR-OK                                                     
014809        MOVE DAT-TIAARP  TO DAT-I-TIDATUM  WS-PERIOD                      
014816     ELSE                                                                 
014817        STRING ' FEL FRÅN DATUMRUTIN WDATKONV A'                          
014818        DELIMITED BY SIZE INTO FELTEXT                                    
014819        CALL FELLOG                                                       
014820     END-IF                                                               
014821                                                                          
014822     MOVE 'AARP'      TO DAT-KDDATFORM                                    
014824                                                                          
014825     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
014826                     DAT-O-TIDATUM DAT-KDSVAR                             
014827                                                                          
014828     IF DAT-KDSVAR-OK                                                     
014829        MOVE DAT-TISEKEL     TO WS-TISEKEL                                
014830        MOVE DAT-TIAAVV-GRP  TO WS-TIAAVV                                 
014831        MOVE WS-TIAAVV       TO START-AAVV                                
014832     ELSE                                                                 
014833        STRING ' FEL FRÅN DATUMRUTIN WDATKONV B'                          
014834        DELIMITED BY SIZE INTO FELTEXT                                    
014835        CALL FELLOG                                                       
014836     END-IF                                                               
014837                                                                          
014838     DISPLAY 'START-AAVV  ' START-AAVV                                    
014839     DISPLAY 'SLUT-VV     ' SLUT-VV                                       
014840     .                                                                    
014841     EJECT                                                                
014842 B-NOLLA-ADDERA SECTION.                                                  
014843                                                                          
014850     MOVE '282'             TO UT-IDPTYP                                  
014860     MOVE WS-TIAAPP         TO UT-TIAAPP                                  
014870     MOVE WS-IDARTNR        TO UT-IDARTNR                                 
014880     MOVE ZERO              TO UT-KVOI-SATS                               
014900                               UT-KVOI-PROG                               
014910                               UT-KVOI-REFILL                             
014950     MOVE START-VV          TO IX                                         
014980     PERFORM UNTIL IX > SLUT-VV                                           
014992        ADD OIGB-AAR-KVOI-PROG   (IX)  TO UT-KVOI-PROG                    
014993        ADD OIGB-AAR-KVOI-REFILL (IX)  TO UT-KVOI-REFILL                  
014994        ADD OIGB-AAR-KVOI-SATS   (IX)  TO UT-KVOI-SATS                    
014998        ADD +1                         TO IX                              
014999     END-PERFORM                                                          
015000     .                                                                    
015100     EJECT                                                                
015200 Z-FINIT SECTION.                                                         
015310     CLOSE W23265                                                         
015401     SKIP2                                                                
015402     MOVE 'S' TO POSTSUM-OPKOD                                            
015410     CALL POSTSUM USING POSTSUM-PARM                                      
015500     .                                                                    
015701     EJECT                                                                
015702 S11-SKRIV-W23265 SECTION.                                                
015703                                                                          
015704     COMPUTE WS-TOTAL = UT-KVOI-SATS                                      
015706                      + UT-KVOI-PROG                                      
015707                      + UT-KVOI-REFILL                                    
015710                                                                          
015711     IF WS-TOTAL > ZERO                                                   
015712        WRITE UT-POST FROM UT-AREA                                        
015713                                                                          
015714        MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                
015715        MOVE 'W23265' TO POSTSUM-FDNAMN                                   
015716        MOVE 'W23264D1' TO POSTSUM-DDNAMN2                                
015717        CALL POSTSUM USING POSTSUM-PARM                                   
015718     END-IF                                                               
015720     .                                                                    
015900     EJECT                                                                
016000 S99-ABEND SECTION.                                                       
016100                                                                          
016201     SKIP2                                                                
016202     MOVE 'S' TO POSTSUM-OPKOD                                            
016210     CALL POSTSUM USING POSTSUM-PARM                                      
016300     CALL ABEND USING RKOD-ABEND                                          
016400     .                                                                    
016500     EJECT                                                                
016600* --- IMS SEKTIONER ---                                                   
016700     SKIP3                                                                
016800 IMS-GET-OIGB   SECTION.                                                  
016810                                                                          
016820     CALL CBLTDLI USING GN WDL8-PCB DLI-IO-AREA                           
016821     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
016822     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
016823     PERFORM IMS-STATUSKONTROLL                                           
016830     .                                                                    
016900     EJECT                                                                
017000 IMS-STATUSKONTROLL SECTION.                                              
017100                                                                          
017200     SET STATUS-IX TO 1                                                   
017300     SEARCH GODK-STATUS                                                   
017400       AT END                                                             
017500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
017600           DELIMITED BY SIZE INTO FELTEXT                                 
017700         DISPLAY FELTEXT                                                  
017800         CALL FELLOG                                                      
017900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
018000         CONTINUE                                                         
018100     END-SEARCH                                                           
018200     .                                                                    
