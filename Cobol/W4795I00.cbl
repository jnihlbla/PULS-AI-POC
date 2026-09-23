000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4795I00.                                                 
000400 AUTHOR.        PER FREDRIKSSON.                                          
000500 DATE-WRITTEN.  NOV 1995                                                  
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*                                                                         
001200*        PROGRAMMET LÄSER WDE4 MED SB.                                    
001300*        SUGER UT INFORMATION OCH SKAPAR EN FIL.                          
001400*                                                                         
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900*                                                                         
002000 FILE-CONTROL.                                                            
002100                                                                          
002200*                                                                         
002300     SELECT W4795I    ASSIGN TO W4795ID1.                                 
002400*                                                                         
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900                                                                          
003000 FD  W4795I                                                               
003100     LABEL RECORD    STANDARD                                             
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS 0.                                                    
003400                                                                          
003500*01  E54-AREA  -COPY W4795I    -L                                         
003600     EJECT                                                                
003700                                                                          
003800 WORKING-STORAGE SECTION.                                                 
003900*    -- CHECKED BY WY2000                                                 
004000     SKIP3                                                                
004100*                                                                         
004200 77   PROGRAM-NAMN               PIC X(6)    VALUE 'W4795I'.              
004300                                                                          
004400 77  JA                          PIC X(1)    VALUE 'J'.                   
004500 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004600 77  SKRIVNA-POSTER              PIC S9(1)   COMP-3.                      
004700*                                                                         
004800 01  WS-IDKUNDRF-X.                                                       
004900   03  WS-IDKUNDRF               PIC 9(5).                                
005000   03  FILLER                    PIC X(5).                                
005100*                                                                         
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300*                                                                         
005400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900                                                                          
006000     EJECT                                                                
006500 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
006600     SKIP2                                                                
006700*01          -COPY W4795I   -PRE E54-                                     
006800     EJECT                                                                
006900 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
007000     SKIP2                                                                
007100 01  IMS-WS.                                                              
007200                                                                          
007300     03  STATUS-WS               PIC X(2).                                
007400        88  SEGMENT-FINNS                    VALUE '  '.                  
007500        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
007600        88  SEGMENT-SLUT                     VALUE 'GB'.                  
007700                                                                          
007800     03  GODK-STATUSKODER.                                                
007900         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
008000                                                                          
008100     03  SSA1                    PIC X(64).                               
008200     EJECT                                                                
008300*01  -COPY W0003                                                          
008400     EJECT                                                                
008500*01  -COPY W0005    -PRE POSTSUM-                                         
008600     EJECT                                                                
008700 01  DLI-IO-AREA.                                                         
008800     03  IO-AREA             PIC X(500).                                  
008900     SKIP3                                                                
009000*    03  WDE401    -COPY WDE401    -RED IO-AREA                           
009100     EJECT                                                                
009200*    03  WDE411    -COPY WDE411    -RED IO-AREA                           
009300     EJECT                                                                
009600 LINKAGE SECTION.                                                         
009700     SKIP3                                                                
009800*01  -COPY W0008   -PRE WDE4-                                             
009900         05  FILLER          PIC X(1).                                    
010000     EJECT                                                                
010100 PROCEDURE DIVISION  USING WDE4-PCB.                                      
010200     ENTRY 'DLITCBL' USING WDE4-PCB.                                      
010300                                                                          
010400     PERFORM A-INIT                                                       
010500     PERFORM IMS-GET-WDE4                                                 
010600                                                                          
010700     PERFORM UNTIL SEGMENT-SLUT                                           
010800                                                                          
010900       EVALUATE WDE4-SEG-NAME-FB                                          
011000         WHEN 'WDE401'                                                    
011100           PERFORM WDE401-SKAPA-E54                                       
011200                                                                          
011300         WHEN 'WDE411'                                                    
011400           PERFORM WDE411-SKRIV-E54                                       
011500                                                                          
011600       END-EVALUATE                                                       
011700                                                                          
011800       PERFORM IMS-GET-WDE4                                               
011900     END-PERFORM                                                          
012000                                                                          
012100     PERFORM Z-FINIT                                                      
012200     MOVE ZERO TO RETURN-CODE                                             
012300     GOBACK                                                               
012400     .                                                                    
012500     EJECT                                                                
012600 A-INIT SECTION.                                                          
012700                                                                          
012800     OPEN OUTPUT W4795I                                                   
012900                                                                          
013000     MOVE PROGRAM-NAMN     TO POSTSUM-PROGNAMN                            
013100     .                                                                    
013200     EJECT                                                                
013300 WDE401-SKAPA-E54       SECTION.                                          
013400                                                                          
013500     MOVE KORD-IDDISTR        TO E54-IDDISTR                              
013600     MOVE KORD-IDKUNDNR       TO E54-IDKUNDNR                             
013700     MOVE KORD-IDKUNDRF       TO E54-IDKUNDRF                             
013800     MOVE KORD-IDPLKLST       TO E54-IDPLKLST                             
013900     MOVE KORD-IDDC           TO E54-IDDC                                 
014000     MOVE KORD-KDFAKTYP       TO E54-KDFAKTYP                             
014100     MOVE KORD-KDFRAKT        TO E54-KDFRAKT                              
014200     MOVE KORD-KDORDKL        TO E54-KDORDKL                              
014300     MOVE KORD-TIORDREG       TO E54-TIORDREG                             
014400     MOVE KORD-IDUSER         TO E54-IDUSER-PACK                          
014500     MOVE KORD-KDPERSON       TO E54-KDPERSON                             
014600                                                                          
014700     IF E54-KDORDKL = 5                                                   
014800       MOVE KORD-IDARTNR-SATS TO E54-IDARTNR-SATS                         
014900     ELSE                                                                 
015000       MOVE ZERO              TO E54-IDARTNR-SATS                         
015100     END-IF                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 WDE411-SKRIV-E54       SECTION.                                          
015500                                                                          
015600     MOVE ORAD-IDPURAD           TO E54-IDRADNR-KO                        
015700     MOVE ORAD-IDPRODNR          TO E54-IDPRODNR                          
015800     MOVE ORAD-IDARTNR           TO E54-IDARTNR                           
015900     MOVE ORAD-FLDIRLEV          TO E54-FLDIRLEV                          
016000     MOVE ORAD-IDKUNDRF-RO       TO E54-IDKUNDRF-RO                       
016100     MOVE ORAD-KDORDTYP          TO E54-KDORDTYP                          
016200     MOVE ORAD-KDPRODSL          TO E54-KDPRODSL                          
016300     MOVE ORAD-IDKONTO           TO E54-IDKONTO                           
016400     MOVE ORAD-IDKST             TO E54-IDKST                             
016500     MOVE ORAD-KVAVBART          TO E54-KVAVBART                          
016600     MOVE ORAD-KVBEART           TO E54-KVBEART                           
016700     MOVE ORAD-KVLEVART          TO E54-KVLEVART                          
016800     MOVE ORAD-PRARTNTO          TO E54-PRARTNTO                          
016810     MOVE ORAD-PRARTNTO-LOC      TO E54-PRARTNTO-LOC                      
016820     MOVE ORAD-PRARTNTO-LOCPREL  TO E54-PRARTNTO-LOCPREL                  
016900     MOVE ORAD-VKARTNTO          TO E54-VKARTNTO                          
017000     MOVE ORAD-VLARTNTO          TO E54-VLARTNTO                          
017100     MOVE ORAD-FLPRTILL          TO E54-FLPRTILL                          
017200     MOVE ORAD-KDRADSTA          TO E54-KDRADSTA                          
017300     MOVE ORAD-KDARTURS          TO E54-KDARTURS                          
017400     MOVE 'E54'                  TO E54-IDPTYP                            
017500                                                                          
017600     WRITE E54-AREA  FROM E54-W4795I                                      
017700                                                                          
017800     MOVE 'W4795I'     TO POSTSUM-FDNAMN                                  
017900     MOVE 'W4795ID1'   TO POSTSUM-DDNAMN2                                 
018000     MOVE 'E54'        TO POSTSUM-TRANSTYP                                
018100                                                                          
018200     CALL POSTSUM USING POSTSUM-PARM                                      
018300     .                                                                    
018400     EJECT                                                                
018500 Z-FINIT  SECTION.                                                        
018600                                                                          
018700     CLOSE W4795I                                                         
018800                                                                          
018900     MOVE 'S'          TO POSTSUM-OPKOD                                   
019000                                                                          
019100     CALL POSTSUM USING POSTSUM-PARM                                      
019200     .                                                                    
019300     EJECT                                                                
019400                                                                          
019500*         * I M S  S E C T I O N                                          
019600                                                                          
019700                                                                          
019800 IMS-GET-WDE4             SECTION.                                        
019900                                                                          
020000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
020100     CALL CBLTDLI USING GN WDE4-PCB IO-AREA                               
020200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
020300     PERFORM IMS-STATUSKONTROLL.                                          
020400     SKIP3                                                                
020500 IMS-STATUSKONTROLL       SECTION.                                        
020600                                                                          
020700     SET STATUS-IX TO 1                                                   
020800     SEARCH GODK-STATUS AT END CALL FELLOG                                
020900     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
021000     END-SEARCH.                                                          
