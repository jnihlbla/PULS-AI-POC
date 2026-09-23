000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W0060600.                                                
000400 AUTHOR.         KJELL ANDRE.                                             
000500     DATE-WRITTEN.   MARS 86.                                             
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        START AV EN BMP I SOP.                                           
001100*        BMP-NAMNET OCH SOPFUNKTION FÅS SOM INDATA I TRANSEN              
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W0T606  W0T606U                                     
001500*        MID:         W0I60601                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W0O60601                                            
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP3                                                                
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002401                                                                          
002410*    -- CHECKED BY WY2000                                                 
002500 01  PROGRAM-NAMN                PIC X(8)    VALUE 'W0060600'.            
002600     SKIP3                                                                
002700 01  GENERELLA-SUBPROGRAM.                                                
002800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
002900     03  W980SOP                 PIC X(8)    VALUE 'W980SOP'.             
003000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
003100     EJECT                                                                
003200 77    JA                        PIC X       VALUE 'J'.                   
003300 77    NEJ                       PIC X       VALUE 'N'.                   
003400 77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +629  COMP SYNC.        
003500                                                                          
003600 77  INDATA-SW                   PIC X.                                   
003700   88  INDATA-OK                 VALUE 'J'.                               
003800     EJECT                                                                
003900 01    MEDDELANDE.                                                        
004000   03    FEL-1                   PIC X(40)   VALUE                        
004100             'ENTER PROCESS NAME'.                                        
004200                                                                          
004300   03    FEL-2.                                                           
004400     05  FILLER                  PIC X(14)   VALUE                        
004500             'ERROR CODE SOP'.                                            
004600     05  FEL-2-MSGCODE           PIC 999.                                 
004700                                                                          
004800   03    INF-1                   PIC X(55)   VALUE                        
004900             'FUNCTION HAS BEEN PERFORMED'.                               
005000                                                                          
005100   03    INF-2                   PIC X(55)   VALUE                        
005200             'PRESS PF11 TO PERFORM FUNCTION'.                            
005300     EJECT                                                                
005400******************************************************************        
005500*                                                                         
005600*                AREOR FÖR W980SOP                                        
005700*                                                                         
005800 01    FILLER                    PIC X(16)   VALUE 'SOP-WS'.              
005900                                                                          
006000*01  -COPY WSOPAREA                                                       
006200     EJECT                                                                
006300******************************************************************        
006400*                                                                         
006500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
006600*                                                                         
006700 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
006800     SKIP3                                                                
006900*01    MID -COPY W0I60601                                                 
007100     EJECT                                                                
007200*01    -COPY WMSGAREA                                                     
007400     EJECT                                                                
007500*  03    MOD -COPY W0O60601  -RED MSG-AREA.                               
007700     EJECT                                                                
007800*01    -COPY WMFSAREA                                                     
008000     EJECT                                                                
008100******************************************************************        
008200*                                                                         
008300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008400*                                                                         
008500 01    IMS-WS.                                                            
008600   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
008700     SKIP3                                                                
008800*                        **** STATUS-KOD FRÅN IMS                         
008900   03    STATUS-WS               PIC XX.                                  
009000     88    SEGMENT-FINNS                     VALUE '  '.                  
009100     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
009200     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
009300     SKIP3                                                                
009400   03    GODK-STATUSKODER.                                                
009500     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
009600     EJECT                                                                
009700*                            IMS FUNKTIONSKODER                           
009800*01    -COPY W0003                                                        
010000     EJECT                                                                
010100 LINKAGE SECTION.                                                         
010200*01    -COPY W0009     -PRE MSG-                                          
010400     EJECT                                                                
010500 PROCEDURE DIVISION USING MSG-PCB.                                        
010600     ENTRY 'DLITCBL' USING MSG-PCB                                        
010700                                                                          
010800                                                                          
010900     PERFORM IMS-GET-MSG                                                  
011000     IF SEGMENT-FINNS                                                     
011100       PERFORM A-INIT                                                     
011200       IF MFS-UPDATE                                                      
011300         PERFORM B-KOLLA-INDATA                                           
011400         IF INDATA-OK                                                     
011500           PERFORM C-STARTA-BMP                                           
011600         END-IF                                                           
011700       ELSE IF MFS-IDTRANS = '0606'                                       
011800         PERFORM D-UPPMANA-TRYCK-PF11                                     
011900       END-IF                                                             
012000       END-IF                                                             
012100       IF MFS-IDTRANS = '0606'                                            
012200           OR NOT MFS-UPDATE                                              
012300           OR SOP-RETCODE > 4                                             
012400         MOVE MAX-MOD-LAENGD TO MSG-KVLL                                  
012500         PERFORM IMS-INSERT-MSG                                           
012600       END-IF                                                             
012700     END-IF                                                               
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK.                                                              
013000     EJECT                                                                
013100 A-INIT   SECTION.                                                        
013200     SKIP2                                                                
013300     IF MSG-DUBBLA-TRANSKODER                                             
013400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I60601                 
013500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
013600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
013700       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
013800     ELSE                                                                 
013900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I60601                  
014000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
014100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
014200       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
014300     END-IF                                                               
014400                                                                          
014500     MOVE LOW-VALUE TO MSG-AREA                                           
014600     MOVE 'W0O60601' TO MFS-IDMOD                                         
014700     MOVE '0606' TO MOD-IDTRANS                                           
014800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
014900                             MOD-TEMFSINF                                 
015000                             MOD-IDPROCESS                                
015100                             MOD-KDSOPFUNK                                
015110                             MOD-TIORDDAT                                 
015200                             MOD-TESYMBV                                  
015300                                                                          
015400     MOVE ZERO TO SOP-RETCODE                                             
015500     .                                                                    
015600     EJECT                                                                
015700 B-KOLLA-INDATA    SECTION.                                               
015800     SKIP2                                                                
015900     MOVE JA TO INDATA-SW                                                 
016000                                                                          
016100     IF MID-IDPROCESS = ALL '+'                                           
016200       MOVE SPACE TO SOP-PROC-NAME                                        
016300       MOVE NEJ TO INDATA-SW                                              
016400       MOVE FEL-1 TO MOD-TEMFSFEL                                         
016500     ELSE                                                                 
016600       MOVE MID-IDPROCESS TO SOP-PROC-NAME                                
016700     END-IF                                                               
016800                                                                          
016900     IF MID-KDSOPFUNK = SPACE OR '+'                                      
017000       MOVE 'O' TO SOP-SOPFUNC                                            
017100     ELSE                                                                 
017200       MOVE MID-KDSOPFUNK TO SOP-SOPFUNC                                  
017300     END-IF                                                               
017400                                                                          
017410     IF MID-TIORDDAT NOT NUMERIC                                          
017420       MOVE ZERO TO SOP-ACTPASS-DATE                                      
017430     ELSE                                                                 
017440       MOVE MID-TIORDDAT TO SOP-ACTPASS-DATE                              
017450     END-IF                                                               
017460                                                                          
017500     IF MID-TESYMBV = SPACE OR '+'                                        
017600       MOVE SPACE TO SOP-SYMBOLIC-VARIABLES                               
017700     ELSE                                                                 
017800       MOVE MID-TESYMBV TO SOP-SYMBOLIC-VARIABLES                         
017900     END-IF                                                               
018000     .                                                                    
018100     EJECT                                                                
018200 C-STARTA-BMP      SECTION.                                               
018300     SKIP2                                                                
018400     MOVE 'W ' TO SOP-DDPREFIX                                            
018600     CALL W980SOP USING SOP-PARM-AREA                                     
018700     IF SOP-RETCODE NOT = ZERO                                            
018800       MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPROCESS-ATTR                    
018900       MOVE SOP-PROC-NAME        TO MOD-IDPROCESS                         
019000       MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDSOPFUNK-ATTR                    
019100       MOVE SOP-SOPFUNC          TO MOD-KDSOPFUNK                         
019110       MOVE MFS-ALFA-FAELT-FEL   TO MOD-TIORDDAT-ATTR                     
019120       MOVE SOP-ACTPASS-DATE     TO MOD-TIORDDAT                          
019200       MOVE MFS-ALFA-FAELT-FEL   TO MOD-TESYMBV-ATTR                      
019300       MOVE SOP-SYMBOLIC-VARIABLES TO MOD-TESYMBV                         
019400       MOVE SOP-MSGCODE          TO FEL-2-MSGCODE                         
019500       MOVE FEL-2                TO MOD-TEMFSFEL                          
019600     ELSE                                                                 
019700       MOVE INF-1 TO MOD-TEMFSINF                                         
019800     END-IF                                                               
019900     .                                                                    
020000     EJECT                                                                
020100 D-UPPMANA-TRYCK-PF11 SECTION.                                            
020200     SKIP2                                                                
020300     MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDPROCESS-ATTR                      
020400     MOVE MFS-ROER-EJ-FAELT    TO MOD-IDPROCESS                           
020500     MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSOPFUNK-ATTR                      
020600     MOVE MFS-ROER-EJ-FAELT    TO MOD-KDSOPFUNK                           
020610     MOVE MFS-NUM-FAELT-RAETT  TO MOD-TIORDDAT-ATTR                       
020620     MOVE MFS-ROER-EJ-FAELT    TO MOD-TIORDDAT                            
020700     MOVE MFS-ALFA-FAELT-RAETT TO MOD-TESYMBV-ATTR                        
020800     MOVE MFS-ROER-EJ-FAELT    TO MOD-TESYMBV                             
020900     MOVE INF-2                TO MOD-TEMFSINF                            
021000     .                                                                    
021100     EJECT                                                                
021200* IMS SEKTIONER                                                           
021300     SKIP3                                                                
021400 IMS-GET-MSG SECTION.                                                     
021500     SKIP2                                                                
021600     MOVE SPACE TO MSG-IO-AREA                                            
021700     MOVE '  QC' TO GODK-STATUSKODER                                      
021800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
021900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022000     PERFORM IMS-STATUSKONTROLL                                           
022100     .                                                                    
022200     EJECT                                                                
022300 IMS-INSERT-MSG SECTION.                                                  
022400     SKIP2                                                                
022500     IF ENGLISH-TEXT                                                      
022600       MOVE 'N' TO MFS-KDHUVOMR                                           
022700     END-IF                                                               
022800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
022900     MOVE SPACE TO GODK-STATUSKODER                                       
023000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
023100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023200     PERFORM IMS-STATUSKONTROLL                                           
023300     .                                                                    
023400     EJECT                                                                
023500 IMS-STATUSKONTROLL SECTION.                                              
023600     SKIP2                                                                
023700     SET STATUS-IX TO 1                                                   
023800     SEARCH GODK-STATUS AT END CALL FELLOG                                
023900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
024000     END-SEARCH                                                           
024100     .                                                                    
