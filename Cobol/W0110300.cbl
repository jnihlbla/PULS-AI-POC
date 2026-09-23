000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0110300.                                                
000300 AUTHOR.         MONICA.                                                  
000400 DATE-WRITTEN.   DEC 1990.                                                
000500*    REMARKS.                                                             
000600     EJECT                                                                
000700 ENVIRONMENT DIVISION.                                                    
000800 INPUT-OUTPUT SECTION.                                                    
000900 FILE-CONTROL.                                                            
001000         SELECT W29968         ASSIGN TO UT-S-W01103D1.                   
001100         SELECT W29968X        ASSIGN TO UT-S-W01103D2.                   
001200                                                                          
001300     EJECT                                                                
001400 DATA DIVISION.                                                           
001500 FILE SECTION.                                                            
001600 FD  W29968                                                               
001700     LABEL RECORD STANDARD                                                
001800     RECORDING MODE F                                                     
001900     BLOCK CONTAINS 0 RECORDS.                                            
002000*        LÄSER ERSÄTTNINGAR. SB.                                          
002100*01 POST    -COPY WSUPERS       -PRE UT68-    -L.                         
002200                                                                          
002300                                                                          
002400 FD  W29968X                                                              
002500     LABEL RECORD STANDARD                                                
002600     RECORDING MODE F                                                     
002700     BLOCK CONTAINS 0 RECORDS.                                            
002800*        LÄSER ERSÄTTNINGAR. SB.                                          
002900*01 POST    -COPY WSUPERSX      -PRE UT68X-   -L.                         
003000                                                                          
003100                                                                          
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600******************************************************************        
003700*    F L A G G O R                                                        
003800******************************************************************        
003900                                                                          
004000 01  FLAGGOR.                                                             
004100     05  SKRIV-SW        PIC X(01)   VALUE SPACE.                         
004200                                                                          
004300******************************************************************        
004400*    K O N S T A N T E R                                                  
004500******************************************************************        
004600                                                                          
004700 01  KONSTANTER.                                                          
004800     05  JA              PIC X(01)   VALUE 'J'.                           
004900     05  NEJ             PIC X(01)   VALUE 'N'.                           
005000     05  PROGRAM-NAMN    PIC X(08)   VALUE 'W01103'.                      
005100     05  OPKOD-KONSTANT  PIC X(01)   VALUE 'S'.                           
005200                                                                          
005300******************************************************************        
005400*    D Y N A M I S K A - S U B P G M                                      
005500******************************************************************        
005600                                                                          
005700 01  DYNAMISKA-SUBPGM.                                                    
005800     05  POSTSUM         PIC X(08)   VALUE 'POSTSUM'.                     
005900     05  CBLTDLI         PIC X(08)   VALUE 'CBLTDLI'.                     
006000     05  FELLOG          PIC X(08)   VALUE 'FELLOG '.                     
006100                                                                          
006200     EJECT                                                                
006300 01  FILLER             PIC X(08)    VALUE 'IMS-WS'.                      
006400 01  IMS-WS.                                                              
006500     03  STATUS-WS      PIC XX.                                           
006600         88  SEGMENT-FINNS           VALUE '  '.                          
006700         88  BASEN-SLUT              VALUE 'GB'.                          
006800     03  GODK-STATUSKODER.                                                
006900         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
007000     SKIP3                                                                
007100**************  IMS-CALL FUNKTIONER                                       
007200*01     -COPY W0003                                                       
007300     EJECT                                                                
007400 01  FILLER        PIC X(16)    VALUE 'IO-AREA'.                          
007500 01  IO-AREA       PIC X(150).                                            
007600*01  A   -COPY WDD701   -PRE WDD701- -RED IO-AREA.                        
007700*01  B   -COPY WDD702   -PRE WDD702- -RED IO-AREA.                        
007800     EJECT                                                                
007900******************************************************************        
008000*    U T P O S T E N                                                      
008100******************************************************************        
008200                                                                          
008300*01  AREA        -COPY WSUPERS    -PRE UT68-                              
008400     EJECT                                                                
008410                                                                          
008420*01  AREA        -COPY WSUPERSX   -PRE UT68X-                             
008430     EJECT                                                                
008600******************************************************************        
008700*    P O S T S U M                                                        
008800******************************************************************        
008900                                                                          
009000*01              -COPY W0005 -PRE POSTSUM-                                
009100     EJECT                                                                
009200                                                                          
009300 LINKAGE SECTION.                                                         
009400*01              -COPY W0008  -PRE WDD7-                                  
009500     05  FILLER   PIC XX.                                                 
009600     EJECT                                                                
009700 PROCEDURE DIVISION USING WDD7-PCB.                                       
009800     ENTRY 'CBLTDLI'  USING WDD7-PCB.                                     
009900     PERFORM A-INIT                                                       
010000     PERFORM IMS-GET-WDD7                                                 
010100                                                                          
010200     PERFORM UNTIL BASEN-SLUT                                             
010300         EVALUATE WDD7-SEG-NAME-FB                                        
010400             WHEN 'WDD701  '                                              
010500                           PERFORM B-MOVE-WDD701                          
010600             WHEN 'WDD702  '                                              
010700                           PERFORM C-MOVE-WDD702                          
010800                           PERFORM D-SKRIV-UT68-POST                      
010810                           PERFORM E-SKRIV-UT68X-POST                     
010900         END-EVALUATE                                                     
011000         PERFORM IMS-GET-WDD7                                             
011100     END-PERFORM                                                          
011200                                                                          
011300     PERFORM Z-FINIT                                                      
011400     MOVE ZERO TO RETURN-CODE                                             
011500     GOBACK.                                                              
011600     EJECT                                                                
011700 A-INIT SECTION.                                                          
011800     OPEN OUTPUT W29968                                                   
011900                 W29968X                                                  
012000                                                                          
012100     MOVE PROGRAM-NAMN       TO POSTSUM-PROGNAMN                          
012200                                                                          
012300     MOVE ZERO               TO UT68-REKSIFFR                             
012400                                UT68-TIERSDAT                             
012500                                UT68-KDERS                                
012600     .                                                                    
012700     EJECT                                                                
012800 B-MOVE-WDD701 SECTION.                                                   
012900                                                                          
013000     MOVE WDD701-IDARTNR     TO UT68-IDARTNR                              
013100     MOVE WDD701-DIERS-ERS   TO UT68-DIERS-ERS                            
013101                                                                          
013110     MOVE WDD701-IDARTNR     TO UT68X-IDARTNR                             
013120     MOVE WDD701-DIERS-ERS   TO UT68X-DIERS-ERS                           
013200     .                                                                    
013300     EJECT                                                                
013400 C-MOVE-WDD702 SECTION.                                                   
013500                                                                          
013600     MOVE WDD702-FLTEXT        TO UT68-FLTEXT                             
013700     MOVE WDD702-IDKORTNR      TO UT68-IDKORTNR                           
013800     MOVE WDD702-TYP1          TO UT68-TYP1                               
013801                                                                          
013810     MOVE WDD702-FLTEXT        TO UT68X-FLTEXT                            
013820     MOVE WDD702-IDKORTNR      TO UT68X-IDKORTNR                          
013821     IF WDD702-FLTEXT = JA                                                
013822       MOVE WDD702-BEERS       TO UT68X-BEERS                             
013823       MOVE ZERO               TO UT68X-IDARTNR-TILLK                     
013824       MOVE ZERO               TO UT68X-DIERS-TILLK                       
013825     ELSE                                                                 
013826       MOVE WDD702-IDARTNR-TILLK TO UT68X-IDARTNR-TILLK                   
013827       MOVE WDD702-DIERS-TILLK TO UT68X-DIERS-TILLK                       
013828       MOVE SPACE              TO UT68X-BEERS                             
013829     END-IF                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 D-SKRIV-UT68-POST SECTION.                                               
014200                                                                          
014300     WRITE UT68-POST FROM UT68-AREA                                       
014400                                                                          
014500     MOVE 'W29968'        TO POSTSUM-FDNAMN                               
014600     MOVE 'W29968D1'      TO POSTSUM-DDNAMN2                              
014700     MOVE SPACE           TO POSTSUM-TRANSTYP                             
014800     CALL POSTSUM USING POSTSUM-PARM                                      
014900     .                                                                    
015000     EJECT                                                                
015010 E-SKRIV-UT68X-POST SECTION.                                              
015020                                                                          
015030     WRITE UT68X-POST FROM UT68X-AREA                                     
015040                                                                          
015050     MOVE 'W29968X'       TO POSTSUM-FDNAMN                               
015060     MOVE 'W29968D2'      TO POSTSUM-DDNAMN2                              
015070     MOVE SPACE           TO POSTSUM-TRANSTYP                             
015080     CALL POSTSUM USING POSTSUM-PARM                                      
015090     .                                                                    
015091     EJECT                                                                
015100 Z-FINIT SECTION.                                                         
015200                                                                          
015300     CLOSE W29968                                                         
015310           W29968X                                                        
015400                                                                          
015500     MOVE OPKOD-KONSTANT  TO POSTSUM-OPKOD                                
015600     CALL POSTSUM USING POSTSUM-PARM                                      
015700     .                                                                    
015800     EJECT                                                                
015900 IMS-GET-WDD7 SECTION.                                                    
016000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
016100     CALL CBLTDLI USING GN WDD7-PCB IO-AREA                               
016200     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
016300     PERFORM IMS-STATUSKONTROLL                                           
016400     .                                                                    
016500     SKIP2                                                                
016600 IMS-STATUSKONTROLL SECTION.                                              
016700     SET STATUS-IX TO 1                                                   
016800     SEARCH GODK-STATUS AT END CALL FELLOG                                
016900         WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                          
017000         CONTINUE                                                         
017100     END-SEARCH                                                           
017200     .                                                                    
