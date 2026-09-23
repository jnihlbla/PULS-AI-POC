000200 ID  DIVISION.                                                            
000300                                                                          
000400 PROGRAM-ID.    W5139700.                                                 
000500*AUTHOR.        KARL JOHAN HANSSON                                        
000600*DATE-WRITTEN.  JULI 1986.                                                
000700                                                                          
000800*REMARKS.                                                                 
001200*    FUNKTION: PROGRAMMET LÄSER WDH1  MED SB.                             
001300*              SKAPAR EN FIL MED INVENTERINGSANMÄRKNINGAR                 
001400*              ENDAST FÖR CDC (LISTTRANSAR).                              
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*--- RAPPORTFILER:                                                        
002300                                                                          
002400     SELECT W51397                       ASSIGN TO UT-S-W51397D1.         
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W51397                                                               
003100     RECORDING  F                                                         
003200     BLOCK CONTAINS 0.                                                    
003300                                                                          
003400*01  UTPOST1 -COPY W5139700   -L.                                         
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(8)    VALUE 'W5139700'.            
004001                                                                          
004002 01  WS-IDARTNR                  PIC S9(9)    COMP-3.                     
004010 01  WS-TISEGKEY                 PIC 9(9).                                
004091                                                                          
004100 01  DYNAMISKA-SUBPROGRAM.                                                
004200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
004500                                                                          
004600*01  -COPY W0005      -PRE POSTSUM-                                       
004700 01  UT-TRANSID.                                                          
004800     03  FILLER                  PIC X(6)    VALUE 'W51397'.              
004900     03  FILLER                  PIC X(8)    VALUE 'W51397D1'.            
005000     03  FILLER                  PIC X(4)    VALUE 'DC11'.                
005100     EJECT                                                                
005200 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
005300                                                                          
005400*01  -COPY W5139700 -PRE UT-                                              
005410     EJECT                                                                
005420 01  FILLER                      PIC X(8)    VALUE 'DC-AREA '.            
005430                                                                          
005440*01  -COPY WWDC99                                                         
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
006800*01  -COPY W0003                                                          
006900     EJECT                                                                
007000 01  DLI-IO-AREA.                                                         
007100     03  IO-AREA              PIC X(150).                                 
007200     SKIP3                                                                
007300*    03  WLINVA01 -COPY WDH101 -RED IO-AREA                               
007400     SKIP3                                                                
007500*    03  WLINVA11 -COPY WDH111 -RED IO-AREA                               
007600     EJECT                                                                
007700 LINKAGE SECTION.                                                         
007800     SKIP3                                                                
007900*01  -COPY W0008   -PRE WDH1-                                             
008000         05  FILLER           PIC X(1).                                   
008100     EJECT                                                                
008200 PROCEDURE DIVISION USING  WDH1-PCB.                                      
008300     ENTRY 'DLITCBL' USING WDH1-PCB.                                      
008400                                                                          
008500     PERFORM A-INIT                                                       
008600                                                                          
008700     PERFORM IMS-GET-WDH1                                                 
008800                                                                          
008900     PERFORM UNTIL SEGMENT-SLUT                                           
009000                                                                          
009100       EVALUATE WDH1-SEG-NAME-FB                                          
009200                                                                          
009300         WHEN 'WDH101'                                                    
009400           MOVE ART-IDARTNR    TO WS-IDARTNR                              
009500         WHEN 'WDH111'                                                    
009600           PERFORM B-SKRIVPOST                                            
009700                                                                          
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
010800     EJECT                                                                
010900 A-INIT SECTION.                                                          
011000                                                                          
011100     OPEN OUTPUT W51397                                                   
011200                                                                          
011300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011400     .                                                                    
011500     EJECT                                                                
011600 B-SKRIVPOST    SECTION.                                                  
011700                                                                          
011701     MOVE INV-IDDC              TO WS-IDDC                                
011703     IF CDC                                                               
011800       IF INV-TEINVANM = SPACE OR LOW-VALUE                               
011900         CONTINUE                                                         
012000       ELSE                                                               
012200         MOVE WS-IDARTNR       TO UT-IDARTNR                              
012300         MOVE INV-IDDC         TO UT-IDDC                                 
012400         MOVE INV-KDINVKAT     TO UT-KDINVKAT                             
012510         MOVE INV-TISEGKEY     TO WS-TISEGKEY                             
012520         MOVE WS-TISEGKEY(3:6) TO UT-TIM-INV                              
012600         MOVE INV-TEINVANM     TO UT-TEINVANM                             
012700         PERFORM S01-SKRIV-W51397                                         
012900       END-IF                                                             
012940     END-IF                                                               
013000     .                                                                    
013100 Z-FINIT  SECTION.                                                        
013200                                                                          
013300     CLOSE W51397                                                         
013400                                                                          
013500     MOVE 'S' TO POSTSUM-OPKOD                                            
013600     CALL POSTSUM USING POSTSUM-PARM                                      
013700     .                                                                    
013800     EJECT                                                                
013900 S01-SKRIV-W51397     SECTION.                                            
014000                                                                          
014100     WRITE UTPOST1 FROM UT-INVANM                                         
014200                                                                          
014300     MOVE UT-TRANSID       TO POSTSUM-TRANSID                             
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
