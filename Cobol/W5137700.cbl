000200 ID  DIVISION.                                                            
000300                                                                          
000400 PROGRAM-ID.    W5137700.                                                 
000500*AUTHOR.        KARL JOHAN HANSSON                                        
000600*DATE-WRITTEN.  MARS 1999.                                                
000700                                                                          
000800*REMARKS.                                                                 
001000*    FUNKTION:                                                            
001200*              PROGRAMMET LÄSER WDH1  MED SB.                             
001300*              SKAPAR EN FIL MED INVENTERINGSANMÄRKNINGAR ÖVER            
001420*              ALLA TRANSAR SOM FINNS UPPLAGDA PÅ WDH111.                 
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002300                                                                          
002400     SELECT W51377                       ASSIGN TO UT-S-W51377D1.         
002500     SKIP2                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003420 FD  W51377                                                               
003430     RECORDING  F                                                         
003440     BLOCK CONTAINS 0.                                                    
003450                                                                          
003460*01  UTPOST  -COPY W51377     -L.                                         
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'W5137700'.            
003900 77  W-IDARTNR                   PIC S9(9)    COMP-3.                     
004001                                                                          
004100 01  DYNAMISKA-SUBPROGRAM.                                                
004200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
004500                                                                          
004600*01  -COPY W0005      -PRE POSTSUM-                                       
004700 01  UT-TRANSID.                                                          
004800     03  FILLER                  PIC X(6)    VALUE 'W51377'.              
004900     03  FILLER                  PIC X(8)    VALUE 'W51377D1'.            
005000     03  UT-IDPTYP               PIC X(4)    VALUE '    '.                
005300                                                                          
005410     EJECT                                                                
005420 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
005430                                                                          
005440*01  -COPY W51377   -PRE UT-                                              
005500     EJECT                                                                
005600 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
005700                                                                          
005800 01  IMS-WS.                                                              
005900                                                                          
006000     03  STATUS-WS               PIC X(2).                                
006100        88  SEGMENT-FINNS                    VALUE '  '.                  
006200        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
006300        88  SEGMENT-SLUT                     VALUE 'GB'.                  
006400                                                                          
006500     03  GODK-STATUSKODER.                                                
006600         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
006700     SKIP3                                                                
006720 01  SSA1                     PIC X(32).                                  
006800*01  -COPY W0003                                                          
006900     EJECT                                                                
006901 01  W-IDDC-B6-X.                                                         
006902     03 W-IDDC-B6             PIC X(2).                                   
006903                                                                          
006910     EJECT                                                                
007000 01  DLI-IO-AREA.                                                         
007100     03  IO-AREA              PIC X(200).                                 
007200     SKIP3                                                                
007300*    03  WLINVA01 -COPY WDH101  -RED IO-AREA                              
007400     SKIP3                                                                
007500*    03  WLINVA11 -COPY WDH111  -RED IO-AREA                              
007510     EJECT                                                                
007520 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
007530 01   DLI-IO-AREA-B601.                                                   
007540*     03  -COPY WDB601                                                    
007550                                                                          
007600     EJECT                                                                
007700 LINKAGE SECTION.                                                         
007800     SKIP3                                                                
007900*01  -COPY W0008   -PRE WDH1-                                             
008000         05  FILLER           PIC X(1).                                   
008010     SKIP3                                                                
008020*01  -COPY W0008   -PRE WDB6-                                             
008030     05  FILLER               PIC X.                                      
008040                                                                          
008100     EJECT                                                                
008200 PROCEDURE DIVISION USING  WDH1-PCB WDB6-PCB.                             
008300     ENTRY 'DLITCBL' USING WDH1-PCB WDB6-PCB.                             
008400                                                                          
008410                                                                          
008500     OPEN OUTPUT W51377                                                   
008510                                                                          
008520     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
008600                                                                          
008700     PERFORM IMS-GET-WDH1                                                 
008900     PERFORM UNTIL SEGMENT-SLUT                                           
009000                                                                          
009100       EVALUATE WDH1-SEG-NAME-FB                                          
009200                                                                          
009300         WHEN 'WDH101'                                                    
009400           MOVE ART-IDARTNR           TO W-IDARTNR                        
009500         WHEN 'WDH111'                                                    
009501           MOVE INV-IDDC              TO UT-IDPTYP                        
009503                                         W-IDDC-B6                        
009504           PERFORM IMS-GU-WDB601                                          
009505                                                                          
009510           IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC OR DCS-NDC                 
009520             MOVE W-IDARTNR           TO UT-IDARTNR                       
009530             MOVE INV-IDDC            TO UT-IDDC                          
009540             MOVE INV-KDINVKAT        TO UT-KDINVKAT                      
009550             PERFORM S01-SKRIV-W51377                                     
009560           END-IF                                                         
009800       END-EVALUATE                                                       
009900                                                                          
010000       PERFORM IMS-GET-WDH1                                               
010100                                                                          
010200     END-PERFORM                                                          
010300                                                                          
010400     PERFORM Z-FINIT                                                      
010500     MOVE ZERO TO RETURN-CODE                                             
010600     GOBACK                                                               
010700     .                                                                    
010800     SKIP3                                                                
013100 Z-FINIT SECTION.                                                         
013200                                                                          
013300     CLOSE W51377                                                         
013400                                                                          
013500     MOVE 'S' TO POSTSUM-OPKOD                                            
013600     CALL POSTSUM USING POSTSUM-PARM                                      
013700     .                                                                    
013800     SKIP3                                                                
013900 S01-SKRIV-W51377 SECTION.                                                
014000                                                                          
014100     WRITE UTPOST  FROM UT-INVPOST                                        
014200                                                                          
014300     MOVE UT-TRANSID      TO POSTSUM-TRANSID                              
014400     CALL POSTSUM USING POSTSUM-PARM                                      
014500     .                                                                    
014600     EJECT                                                                
014700*         * I M S  S E C T I O N                                          
014800                                                                          
014900 IMS-GET-WDH1         SECTION.                                            
015000                                                                          
015100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015200     CALL CBLTDLI USING GN WDH1-PCB IO-AREA                               
015300     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
015400     PERFORM IMS-STATUSKONTROLL                                           
015500     .                                                                    
015510     SKIP3                                                                
015520 IMS-GU-WDB601    SECTION.                                                
015530     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
015540          DELIMITED BY SIZE INTO SSA1                                     
015550     MOVE '  ' TO GODK-STATUSKODER                                        
015560     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
015570     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
015580     PERFORM IMS-STATUSKONTROLL                                           
015590     .                                                                    
015600     SKIP3                                                                
015700 IMS-STATUSKONTROLL   SECTION.                                            
015800                                                                          
015900     SET STATUS-IX TO 1                                                   
016000     SEARCH GODK-STATUS                                                   
016100          AT END                                                          
016200            CALL FELLOG                                                   
016300          WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                        
016400            CONTINUE                                                      
016500     END-SEARCH                                                           
016600     .                                                                    
