000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W2190100.                                                 
000400 AUTHOR.        SVANTE BJÖRKBERG.                                         
000500 DATE-WRITTEN.  OKT 1991.                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
001000*        PROGRAMMET LÄSER WDM2.                                           
001100*        SUGER UT INFORMATION OCH SKAPAR EN FIL FRÅN WDM211.              
001200*                                                                         
001201* ÄNDRINGAR:                                                              
001210*  E-TRACKER: 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2        
001220*                                                                         
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700*                                                                         
001800 FILE-CONTROL.                                                            
001900                                                                          
002000*                                                                         
002100     SELECT W21901    ASSIGN TO W21901D1.                                 
002200*                                                                         
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP2                                                                
002600 FILE SECTION.                                                            
002700                                                                          
002800 FD  W21901                                                               
002900     LABEL RECORD    STANDARD                                             
003000     RECORDING       F                                                    
003100     BLOCK CONTAINS 0.                                                    
003200                                                                          
003300*01  UT-AREA  -COPY W219001   -L                                          
003400     EJECT                                                                
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900*                                                                         
004000 01   PROGRAM-NAMN               PIC X(6)    VALUE 'W47950'.              
004100                                                                          
004200 01  JA                          PIC X(1)    VALUE 'J'.                   
004300 01  NEJ                         PIC X(1)    VALUE 'N'.                   
004500 01  SPAR-IDKAMPRF               PIC S9(7)   COMP-3.                      
004510 01  SPAR-IDDC                   PIC X(02).                               
004520 01  SPAR-TISTADAT               PIC S9(7)   COMP-3.                      
004530 01  SPAR-TISTODAT               PIC S9(7)   COMP-3.                      
004540 01  SPAR-TIREGDAT               PIC S9(7)   COMP-3.                      
004600*                                                                         
004700 01  DYNAMISKA-SUBPROGRAM.                                                
004800*                                                                         
004900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200     EJECT                                                                
005300 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
005400     SKIP2                                                                
005500 01  -COPY W219001  -PRE UT-                                              
005600     EJECT                                                                
005700 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
005800                                                                          
005900 01  IMS-WS.                                                              
006000                                                                          
006100     03  STATUS-WS               PIC X(2).                                
006200        88  SEGMENT-FINNS                    VALUE '  '.                  
006300        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
006400        88  SEGMENT-SLUT                     VALUE 'GB'.                  
006500                                                                          
006600     03  GODK-STATUSKODER.                                                
006700         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
006800                                                                          
006900     EJECT                                                                
007000*01  -COPY W0003                                                          
007100     EJECT                                                                
007200*01  -COPY W0005    -PRE POSTSUM-                                         
007300     EJECT                                                                
008010*    ---  DLI INPUT-OUTPUT AREA                                           
008020                                                                          
008030 01  DLI-IO-WDM2.                                                         
008040     03  IO-AREA-WDM2        PIC X(100).                                  
008060*    03            -COPY WDM201    -RED IO-AREA-WDM2                      
008070     EJECT                                                                
008080*    03            -COPY WDM211    -RED IO-AREA-WDM2                      
008090     EJECT                                                                
008200 LINKAGE SECTION.                                                         
008300     SKIP3                                                                
008400*01  -COPY W0008   -PRE WDM2-                                             
008500         05  FILLER          PIC X(1).                                    
008600     EJECT                                                                
008700 PROCEDURE DIVISION  USING WDM2-PCB.                                      
008800     ENTRY 'DLITCBL' USING WDM2-PCB.                                      
008900                                                                          
009000     PERFORM A-INIT                                                       
009100                                                                          
009110     PERFORM IMS-GN-WDM2                                                  
009200                                                                          
009300     PERFORM UNTIL SEGMENT-SLUT                                           
009400                                                                          
009500       EVALUATE WDM2-SEG-NAME-FB                                          
009600         WHEN 'WDM201'                                                    
009610           MOVE KAMP-IDKAMPRF  TO SPAR-IDKAMPRF                           
009620           MOVE KAMP-IDDC      TO SPAR-IDDC                               
009630           MOVE KAMP-TISTADAT  TO SPAR-TISTADAT                           
009640           MOVE KAMP-TISTODAT  TO SPAR-TISTODAT                           
009650           MOVE KAMP-TIREGDAT  TO SPAR-TIREGDAT                           
009800                                                                          
009900         WHEN 'WDM211'                                                    
010000           PERFORM C-SKRIVA-UT-POST                                       
010100       END-EVALUATE                                                       
010200                                                                          
010310       PERFORM IMS-GN-WDM2                                                
010400     END-PERFORM                                                          
010500                                                                          
010600     PERFORM Z-FINIT                                                      
010700     MOVE ZERO TO RETURN-CODE                                             
010800     GOBACK                                                               
010900     .                                                                    
011000     EJECT                                                                
011100 A-INIT SECTION.                                                          
011200                                                                          
011300     OPEN OUTPUT W21901                                                   
011400                                                                          
011500     MOVE PROGRAM-NAMN     TO POSTSUM-PROGNAMN                            
011600     .                                                                    
011700     EJECT                                                                
012800 C-SKRIVA-UT-POST                     SECTION.                            
012810                                                                          
013000     MOVE SPAR-IDKAMPRF          TO UT-IDKAMPRF                           
013010     MOVE SPAR-IDDC              TO UT-IDDC                               
013020     MOVE SPAR-TISTADAT          TO UT-TISTADAT                           
013030     MOVE SPAR-TISTODAT          TO UT-TISTODAT                           
013040     MOVE SPAR-TIREGDAT          TO UT-TIREGDAT                           
013100     MOVE KART-IDARTNR           TO UT-IDARTNR                            
013310     MOVE KART-KVBEART-KAMP      TO UT-KVBEART-KAMP                       
013311     MOVE KART-KVBEART-KUND      TO UT-KVBEART-KUND                       
013312     MOVE KART-KVBEART-TPO4      TO UT-KVBEART-TPO4                       
013320     MOVE KART-KVRESS-ART        TO UT-KVRESS-ART                         
013321     MOVE KART-KVRESS-KAMP       TO UT-KVRESS-KAMP                        
013322     MOVE KART-TIRES             TO UT-TIRES                              
013330                                                                          
013400     WRITE UT-AREA  FROM UT-W219001                                       
013500                                                                          
013600     MOVE 'W21901'               TO POSTSUM-FDNAMN                        
013700     MOVE 'W21901D1'             TO POSTSUM-DDNAMN2                       
013800     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
013900                                                                          
014000     CALL POSTSUM USING POSTSUM-PARM                                      
014200     .                                                                    
014300     EJECT                                                                
014400 Z-FINIT  SECTION.                                                        
014500                                                                          
014600     CLOSE W21901                                                         
014700                                                                          
014800     MOVE 'S'          TO POSTSUM-OPKOD                                   
014900                                                                          
015000     CALL POSTSUM USING POSTSUM-PARM                                      
015100     .                                                                    
015200     EJECT                                                                
015300                                                                          
015400*         * I M S  S E C T I O N                                          
015500                                                                          
015600                                                                          
015700 IMS-GN-WDM2             SECTION.                                         
015800                                                                          
015900     MOVE '  GAGKGB'       TO GODK-STATUSKODER                            
016000     CALL CBLTDLI USING GN WDM2-PCB IO-AREA-WDM2                          
016100     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
016200     PERFORM IMS-STATUSKONTROLL                                           
016300     .                                                                    
016400                                                                          
016500 IMS-STATUSKONTROLL       SECTION.                                        
016600                                                                          
016700     SET STATUS-IX TO 1                                                   
016800     SEARCH GODK-STATUS AT END CALL FELLOG                                
016900     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
017000     END-SEARCH.                                                          
