000200 ID  DIVISION.                                                            
000400 PROGRAM-ID.    W5604600.                                                 
000500 AUTHOR.        GUN LÖFGREN.                                              
000600 DATE-WRITTEN.  FEBRUARI 1997.                                            
000610 DATE-COMPILED.                                                           
000700                                                                          
001000*    FUNKTION:                                                            
001100*                                                                         
001200*        PROGRAMMET LÄSER WDH7 MED SB.                                    
001300*        SKAPAR EN FIL MED SAMTLIGA JUSTERINGAR UNDER VECKAN              
001310*        GJORDA PÅ NDC 41,42,43 OCH 51 .                                  
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*--- RAPPORTFIL:                                                          
002200                                                                          
002300     SELECT W56046                       ASSIGN TO UT-S-W56046D1.         
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W56046                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS 0.                                                    
003300                                                                          
003400*01  UTPOST -COPY W56046     -L.                                          
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003801                                                                          
003810*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(8)  VALUE 'W5604600'.              
004000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(3) VALUE +16.                     
004100                                                                          
004200 01  DYNAMISKA-SUBPROGRAM.                                                
004300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
004400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
004700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
004710     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
004800                                                                          
004900 01  DAGENS-TIAAVV.                                                       
005100     03  DAGENS-AA               PIC 9(2).                                
005200     03  DAGENS-VV               PIC 9(2).                                
005900     EJECT                                                                
006000*01  -COPY W0005      -PRE POSTSUM-                                       
006200 01  UT-TRANSID.                                                          
006300     03  FILLER                  PIC X(6)  VALUE 'W56046'.                
006400     03  FILLER                  PIC X(8)  VALUE 'W56046D1'.              
006500     03  FILLER                  PIC X(4)  VALUE 'POST'.                  
006600     EJECT                                                                
006601*    --- VALID IDDC CODES                                                 
006602*                                                                         
006603*01  -COPY WWDC99                                                         
006610     EJECT                                                                
006700 01  DATUMKORT-ID                PIC X(6)  VALUE 'WDATUM'.                
006800*01  -COPY WDATKORT                                                       
006900     EJECT                                                                
007100*01  -COPY WDATAREA                                                       
007300     EJECT                                                                
007400 01  FILLER                      PIC X(8)    VALUE 'UT-AREOR'.            
007500                                                                          
007600*01  POST -COPY W56046   -PRE UT-                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
008000                                                                          
008100 01  IMS-WS.                                                              
008200                                                                          
008300     03  STATUS-WS               PIC X(2).                                
008400        88  SEGMENT-SLUT                     VALUE 'GB'.                  
008500        88  SEGMENT-FINNS                    VALUE '  ' 'GA' 'GK'.        
008600        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
008700                                                                          
008800     03  GODK-STATUSKODER.                                                
008900         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
009000                                                                          
009100*01  -COPY W0003                                                          
009300     EJECT                                                                
009400 01  DLI-IO-AREA.                                                         
009500     03  IO-AREA              PIC X(150).                                 
009600     SKIP3                                                                
009700*    03  FILLER -COPY WDH701             -RED IO-AREA                     
009900     EJECT                                                                
010000*    03  FILLER -COPY WDH711             -RED IO-AREA                     
010100     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000     SKIP3                                                                
011100*01  -COPY W0008   -PRE WDH7-                                             
011300         05  FILLER           PIC X(1).                                   
011400     EJECT                                                                
011500 PROCEDURE DIVISION USING  WDH7-PCB.                                      
011600     ENTRY 'DLITCBL' USING WDH7-PCB.                                      
011700                                                                          
011800     PERFORM A-INIT                                                       
011810                                                                          
011900     PERFORM IMS-GET-WDH7                                                 
011910                                                                          
012000     PERFORM UNTIL SEGMENT-SLUT                                           
012010                                                                          
012100        EVALUATE WDH7-SEG-NAME-FB                                         
012200           WHEN 'WDH701  '                                                
012210             MOVE INVA-IDARTNR TO UT-IDARTNR                              
012300                                                                          
012600           WHEN 'WDH711  '                                                
012601             MOVE INVH-IDDC         TO WS-IDDC                            
012602             IF NDC-NA                                                    
012603               PERFORM B-SKRIV-POST                                       
012604             END-IF                                                       
012610                                                                          
012700        END-EVALUATE                                                      
012710                                                                          
012800        PERFORM IMS-GET-WDH7                                              
012810                                                                          
012900     END-PERFORM                                                          
012910                                                                          
013000     PERFORM Z-FINIT                                                      
013100     MOVE ZERO TO RETURN-CODE                                             
013200     GOBACK                                                               
013300     .                                                                    
013400     EJECT                                                                
013500 A-INIT SECTION.                                                          
013600                                                                          
013700     OPEN OUTPUT W56046                                                   
013800                                                                          
013900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000                                                                          
014100     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
014200                                                                          
014300     MOVE D-AAR             TO DAGENS-AA                                  
014400     MOVE D-VECKA           TO DAGENS-VV                                  
014500                                                                          
014600     DISPLAY 'DAGENS ÅR            : ' DAGENS-AA                          
014700     DISPLAY 'DAGENS VECKA         : ' DAGENS-VV                          
014800     .                                                                    
014900     EJECT                                                                
015000 B-SKRIV-POST SECTION.                                                    
015100                                                                          
015110     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
015120     MOVE INVH-DAREGDAT-CLO(3:6) TO DAT-I-TIDATUM                         
015130                                                                          
015140     CALL WDATKONV USING DAT-KDDATFORM                                    
015150                         DAT-I-TIDATUM                                    
015160                         DAT-O-TIDATUM                                    
015170                         DAT-KDSVAR                                       
015200                                                                          
015300     IF (DAT-TIAA = DAGENS-AA) AND                                        
015400        (DAT-TIVV = DAGENS-VV)                                            
015401                                                                          
015420         MOVE ZERO               TO UT-PRAVCOST                           
015430         MOVE INVH-IDDC          TO UT-IDDC                               
015500         MOVE INVH-KVJUSTKV      TO UT-KVJUSTKV                           
015600         MOVE INVH-KDJUSTYP      TO UT-KDJUSTYP                           
015610                                                                          
015700         PERFORM S01-SKRIV-W56046                                         
015800     END-IF                                                               
015900     .                                                                    
016000 Z-FINIT  SECTION.                                                        
016100                                                                          
016200     CLOSE W56046                                                         
016300                                                                          
016400     MOVE 'S' TO POSTSUM-OPKOD                                            
016500     CALL POSTSUM USING POSTSUM-PARM                                      
016600     .                                                                    
016700     SKIP2                                                                
016800 S01-SKRIV-W56046     SECTION.                                            
016900                                                                          
017000     WRITE UTPOST FROM UT-POST                                            
017100                                                                          
017200     MOVE UT-TRANSID      TO POSTSUM-TRANSID                              
017300     CALL POSTSUM USING POSTSUM-PARM                                      
017400     .                                                                    
017500     EJECT                                                                
017600*         * I M S  S E C T I O N                                          
017700                                                                          
017800 IMS-GET-WDH7         SECTION.                                            
017900                                                                          
018000     MOVE '  GAGKGBGAGK' TO GODK-STATUSKODER                              
018100     CALL CBLTDLI USING GN WDH7-PCB IO-AREA                               
018200     MOVE WDH7-STATUS-CODE TO STATUS-WS                                   
018300     PERFORM IMS-STATUSKONTROLL                                           
018400     .                                                                    
018500     SKIP3                                                                
018600 IMS-STATUSKONTROLL   SECTION.                                            
018700                                                                          
018800     SET STATUS-IX TO 1                                                   
018900     SEARCH GODK-STATUS                                                   
019000       AT END CALL FELLOG                                                 
019100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
019200     END-SEARCH                                                           
019300     .                                                                    
