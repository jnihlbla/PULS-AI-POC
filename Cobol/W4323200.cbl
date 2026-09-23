000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4323200.                                                 
000400 AUTHOR.        BO SVENSSON.                                              
000500 DATE-WRITTEN.  MARS 1997.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        PROGRAMMET ÄR ETT SB-PGM.INGÅENDE I RUTINEN(W432D1).             
001100*                                                                         
001200*        PGM. LÄSER WDB2.                                                 
001300*        SKAPAR EN UTFIL(W43232) TILL VADIS                               
001400*                                                                         
001500*    DB.                                                                  
001600*        DB              WDB2   (SB)                                      
001700*        DB              WDB1   (DLI)                                     
001800*                                                                         
001900*    UTDATA.                                                              
002000*        FIL             W43232                                           
002100                                                                          
002200     EJECT                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600     SKIP2                                                                
002700 FILE-CONTROL.                                                            
002800                                                                          
002900     SELECT W43232    ASSIGN TO W43232D1.                                 
003000                                                                          
003030     SELECT W43232X   ASSIGN TO W43232D2.                                 
003040                                                                          
003100 DATA DIVISION.                                                           
003200 FILE SECTION.                                                            
003300                                                                          
003400 FD  W43232                                                               
003500     LABEL RECORD    STANDARD                                             
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  UTVAD-POST  -COPY W432VAD1  -L                                       
004000     SKIP3                                                                
004171 FD  W43232X                                                              
004172     RECORDING       F                                                    
004173     BLOCK CONTAINS  0.                                                   
004174                                                                          
004175*01  POST  -COPY W432320X  -PRE  UTGMTX- -L.                              
004176                                                                          
004180                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400*    -- CHECKED BY WY2000                                                 
004500*                                                                         
004600 77  PROGRAM-NAMN                PIC X(08) VALUE 'W4323200'.              
004700                                                                          
004800 77  JA                          PIC X(1)    VALUE 'J'.                   
004900 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004910 77  IX                          PIC S9(3)   VALUE ZERO.                  
004920 77  IX-DCCLEAR-MAX              PIC S9(3)   VALUE +99  COMP SYNC.        
005000*                                                                         
005100 01  FILLER                      PIC X(8)    VALUE 'UT-AREA '.            
005200     SKIP2                                                                
005300*01  W43232  -COPY W432VAD1  -PRE  VAD-                                   
005405*                                                                         
005406 01  FILLER                      PIC X(8)    VALUE 'UTW43232'.            
005407     SKIP2                                                                
005420     EJECT                                                                
005461 01  FILLER.                                                              
005480*    03  WDB201X   -COPY W432320X                                         
005490     EJECT                                                                
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005600*                                                                         
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000                                                                          
006100     EJECT                                                                
006200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006300*                                                                         
006400     SKIP2                                                                
006500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006600                                                                          
006700 01  NYCKLAR-TILL-DLI.                                                    
006800     03 W-WDB101KY-X.                                                     
006900         05 W-IDPARTNR              PIC X(9) VALUE SPACE.                 
007000         05 W-IDFTG                 PIC 9(2) VALUE ZERO.                  
007100*                                                                         
007200 01  IMS-WS.                                                              
007300                                                                          
007400     03  STATUS-WS               PIC X(2).                                
007500        88  END-OF-DATA                      VALUE 'GB'.                  
007600        88  SEGMENT-FINNS                    VALUE '  '.                  
007700        88  SEGMENT-SAKNAS                   VALUE 'GE'.                  
007800                                                                          
007900     03  GODK-STATUSKODER.                                                
008000         05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).          
008100     SKIP3                                                                
008200 01  SSA1                        PIC X(64).                               
008300                                                                          
008400     EJECT                                                                
008500*01  -COPY W0003                                                          
008600     EJECT                                                                
008700*01  -COPY W0005         -PRE POSTSUM-.                                   
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)   VALUE                        
009000                                             'WDB201-AREA'.               
009100 01  DLI-IO-AREA.                                                         
009200*    03  WDB201    -COPY WDB201                                           
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)    VALUE 'WDB1-AREA'.          
009500 01  WDB1-AREA.                                                           
009600*    03  -COPY WDB101                                                     
009700     EJECT                                                                
009800                                                                          
009900 LINKAGE SECTION.                                                         
010000     SKIP3                                                                
010100*01  -COPY W0008         -PRE WDB2-                                       
010200     05  FILLER           PIC X.                                          
010300*01  -COPY W0008         -PRE WDB1-                                       
010400     05  FILLER           PIC X.                                          
010500     EJECT                                                                
010600 PROCEDURE DIVISION  USING WDB2-PCB WDB1-PCB.                             
010700     ENTRY 'DLITCBL' USING WDB2-PCB WDB1-PCB.                             
010800                                                                          
010900     PERFORM A-INIT                                                       
011000     PERFORM IMS-GN-WDB2                                                  
011100     PERFORM UNTIL END-OF-DATA                                            
011200       EVALUATE WDB2-SEG-NAME-FB                                          
011300         WHEN 'WDB201'                                                    
011400           PERFORM B-SKAPA-FIL                                            
011500       END-EVALUATE                                                       
011600       PERFORM IMS-GN-WDB2                                                
011700     END-PERFORM                                                          
011800                                                                          
011900     PERFORM Z-FINIT                                                      
012000     MOVE ZERO TO RETURN-CODE                                             
012100     GOBACK                                                               
012200     .                                                                    
012300     EJECT                                                                
012400 A-INIT SECTION.                                                          
012500                                                                          
012600     OPEN OUTPUT W43232                                                   
012610          OUTPUT W43232X                                                  
012700                                                                          
012800     MOVE PROGRAM-NAMN     TO POSTSUM-PROGNAMN                            
012900     .                                                                    
013000 B-SKAPA-FIL              SECTION.                                        
013100                                                                          
013200**** HÄMTA IDLANDX2 IFRÅN WDB1 ***                                        
013300     MOVE GMT-IDPARTNR OF GMT-WDB201      TO W-IDPARTNR                   
013400     MOVE GMT-IDFTG    OF GMT-WDB201      TO W-IDFTG                      
013500     PERFORM IMS-GU-WDB1                                                  
013600     IF SEGMENT-FINNS                                                     
013700       MOVE BET-IDLANDX2 TO VAD-IDLANDX2                                  
013800     END-IF                                                               
013900                                                                          
014000     IF  GMT-TISTADAT OF GMT-WDB201> 0                                    
014100     AND BET-IDLANDX2 NOT = '  '                                          
014200       MOVE 'WGM'           TO VAD-IDPTYP                                 
014300       MOVE GMT-IDDISTR    OF GMT-WDB201 TO VAD-IDDISTR                   
014400       MOVE GMT-IDKUNDNR   OF GMT-WDB201 TO VAD-IDKUNDNR                  
014500       MOVE GMT-BEGMT-RAD1 OF GMT-WDB201 TO VAD-BEGMT-RAD1                
014600       MOVE GMT-TISTADAT   OF GMT-WDB201 TO VAD-DASTADAT                  
014700       MOVE GMT-TISTODAT   OF GMT-WDB201 TO VAD-DASTODAT                  
014800                                                                          
014900       IF  VAD-DASTADAT > 500000                                          
015000           ADD 19000000     TO VAD-DASTADAT                               
015100       ELSE                                                               
015200           IF  VAD-DASTADAT >  0                                          
015300               ADD 20000000 TO VAD-DASTADAT                               
015400           END-IF                                                         
015500       END-IF                                                             
015600                                                                          
015700       IF  VAD-DASTODAT > 500000                                          
015800           ADD 19000000     TO VAD-DASTODAT                               
015900       ELSE                                                               
016000           IF  VAD-DASTODAT >  0                                          
016100               ADD 20000000 TO VAD-DASTODAT                               
016200           END-IF                                                         
016300       END-IF                                                             
016400                                                                          
016500       WRITE UTVAD-POST   FROM VAD-W43232                                 
016600                                                                          
016700       MOVE 'W43232'     TO POSTSUM-FDNAMN                                
016800       MOVE 'W43232D1'   TO POSTSUM-DDNAMN2                               
016900       MOVE 'WGM'        TO POSTSUM-TRANSTYP                              
017000                                                                          
017100       CALL POSTSUM USING POSTSUM-PARM                                    
017200     END-IF                                                               
017300     PERFORM S01-WRITE-W43232X                                            
017310     .                                                                    
017400     EJECT                                                                
017500                                                                          
017510 S01-WRITE-W43232X   SECTION.                                             
017520                                                                          
017521     MOVE CORR GMT-WDB201 TO GMT-WDB201X                                  
017522                                                                          
017523     MOVE GMT-ADGMT-GATA OF GMT-WDB201  TO                                
017524          GMT-ADGMT-GATA OF GMT-WDB201X                                   
017525                                                                          
017526     MOVE GMT-ADGMT-LAND OF GMT-WDB201  TO                                
017527          GMT-ADGMT-LAND OF GMT-WDB201X                                   
017528                                                                          
017529     IF GMT-KDPOSTNR OF GMT-WDB201 = 'L'                                  
017530       MOVE GMT-ADGMT-PADR OF GMT-WDB201(1:10)  TO                        
017531            GMT-ADPOSTNR  OF GMT-WDB201X                                  
017532       MOVE GMT-ADGMT-PADR OF GMT-WDB201(11:25) TO                        
017533            GMT-ADCITY    OF GMT-WDB201X                                  
017534     ELSE                                                                 
017535       MOVE GMT-ADGMT-PADR OF GMT-WDB201(26:10) TO                        
017536            GMT-ADPOSTNR  OF GMT-WDB201X                                  
017537       MOVE GMT-ADGMT-PADR OF GMT-WDB201(1:25)  TO                        
017538            GMT-ADCITY    OF GMT-WDB201X                                  
017539     END-IF                                                               
017540                                                                          
017541     MOVE 1 TO IX                                                         
017542     PERFORM UNTIL IX > IX-DCCLEAR-MAX                                    
017543        MOVE GMT-IDDC-BULK OF GMT-WDB201(IX) TO                           
017544             GMT-IDDC-BULK OF GMT-WDB201X(IX)                             
017545        MOVE GMT-IDDC-DAY  OF GMT-WDB201(IX) TO                           
017546             GMT-IDDC-DAY  OF GMT-WDB201X(IX)                             
017547        MOVE GMT-IDDC-VOR  OF GMT-WDB201(IX) TO                           
017548             GMT-IDDC-VOR  OF GMT-WDB201X(IX)                             
017549        ADD 1 TO IX                                                       
017550     END-PERFORM                                                          
017551                                                                          
017552     MOVE 1 TO IX                                                         
017553     PERFORM UNTIL IX > 4                                                 
017554        MOVE GMT-IDDC-RFS    OF GMT-WDB201(IX) TO                         
017555             GMT-IDDC-RFS    OF GMT-WDB201X(IX)                           
017556        MOVE GMT-KVDAGAR-RFS OF GMT-WDB201(IX) TO                         
017557             GMT-KVDAGAR-RFS OF GMT-WDB201X(IX)                           
017558        ADD 1 TO IX                                                       
017559     END-PERFORM                                                          
017560                                                                          
017561     MOVE 1 TO IX                                                         
017562     PERFORM UNTIL IX > 16                                                
017563        MOVE GMT-IDDC-TVSVOR OF GMT-WDB201(IX) TO                         
017564             GMT-IDDC-TVSVOR OF GMT-WDB201X(IX)                           
017565        ADD 1 TO IX                                                       
017566     END-PERFORM                                                          
017567                                                                          
017568     MOVE 1 TO IX                                                         
017569     PERFORM UNTIL IX > 3                                                 
017570        MOVE GMT-IDDC-RET72  OF GMT-WDB201(IX) TO                         
017571             GMT-IDDC-RET72  OF GMT-WDB201X(IX)                           
017572        ADD 1 TO IX                                                       
017573     END-PERFORM                                                          
017574                                                                          
017575     MOVE 1 TO IX                                                         
017576     PERFORM UNTIL IX > 8                                                 
017577        MOVE GMT-IDDC-PREPLAN OF GMT-WDB201(IX) TO                        
017578             GMT-IDDC-PREPLAN OF GMT-WDB201X(IX)                          
017579        ADD 1 TO IX                                                       
017580     END-PERFORM                                                          
017581                                                                          
017590     MOVE GMT-WDB201X     TO UTGMTX-POST                                  
017790     WRITE UTGMTX-POST                                                    
017791                                                                          
017792     MOVE 'W43232X'       TO POSTSUM-FDNAMN                               
017793     MOVE 'W43232D2'      TO POSTSUM-DDNAMN2                              
017794     MOVE 'UTGMTX'        TO POSTSUM-TRANSTYP                             
017795                                                                          
017796     CALL POSTSUM USING POSTSUM-PARM                                      
017797     .                                                                    
017798     EJECT                                                                
017811 Z-FINIT  SECTION.                                                        
017812                                                                          
017813     CLOSE W43232                                                         
017830           W43232X                                                        
017900                                                                          
018000     MOVE 'S'          TO POSTSUM-OPKOD                                   
018100                                                                          
018200     CALL POSTSUM USING POSTSUM-PARM                                      
018300     .                                                                    
018400     EJECT                                                                
018500 IMS-GN-WDB2              SECTION.                                        
018600                                                                          
018700     MOVE '  GAGKGB'       TO GODK-STATUSKODER                            
018800     CALL CBLTDLI USING GN WDB2-PCB DLI-IO-AREA                           
018900     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
019000     PERFORM IMS-STATUSKONTROLL                                           
019100     .                                                                    
019200     SKIP3                                                                
019300 IMS-GU-WDB1 SECTION.                                                     
019400     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
019500                                                                          
019600            DELIMITED BY SIZE INTO SSA1                                   
019700     MOVE '  GE' TO GODK-STATUSKODER                                      
019800     CALL CBLTDLI USING GU WDB1-PCB WDB1-AREA SSA1                        
019900     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
020000     PERFORM IMS-STATUSKONTROLL                                           
020100                                                                          
020200     MOVE 'W43232'     TO POSTSUM-FDNAMN                                  
020300     MOVE 'WDB101  '   TO POSTSUM-DDNAMN2                                 
020400     MOVE 'WDB1'       TO POSTSUM-TRANSTYP                                
020500                                                                          
020600     CALL POSTSUM USING POSTSUM-PARM                                      
020700     SKIP3                                                                
020800     .                                                                    
020900     EJECT                                                                
021000 IMS-STATUSKONTROLL       SECTION.                                        
021100                                                                          
021200     SET STATUS-IX TO 1                                                   
021300     SEARCH GODK-STATUS                                                   
021400       AT END CALL FELLOG                                                 
021500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
021600     END-SEARCH                                                           
021700     .                                                                    
