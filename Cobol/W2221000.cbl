000400 ID DIVISION.                                                             
000500 PROGRAM-ID.                 W2221000.                                    
000900*AUTHOR.                     INGVAR CARLSSON, IDK.                        
001000*DATE-WRITTEN.               NOVEMBER 1978.                               
001100*    SKIP2                                                                
001200*REMARKS.                                                                 
001300                                                                          
001400*    SYSTEM.                                                              
001500*        PROGNOSSYSTEM.                                                   
001600                                                                          
001700*    FUNKTION.                                                            
001800*        LÄSER INFILEN W11126 SOM INNEHÅLLER HÄNDELSER                    
001900*        MED HTYP 2208.                                                   
002000*        POSTERNA                                                         
002100*                 SORTERAS DE TILLSAMMANS MED POSTERNA PÅ                 
002200*        INFILEN W09257 OCH SKRIVS PÅ UTFILEN W22211                      
002300*        MED SORTERINGSORDNINGEN: IDARTNR, KDCLAGER,IDPTYP.               
003000                                                                          
003400     EJECT                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600 INPUT-OUTPUT SECTION.                                                    
003700 FILE-CONTROL.                                                            
003800     SKIP2                                                                
003900*--------------------------------------- PROGNOSUPPDATERINGS-             
004000*                                        TRANSAR                          
004100*                                        INPUT                            
004200                                                                          
004300     SELECT W09257 ASSIGN W22210D1.                                       
004310     SKIP2                                                                
004320*--------------------------------------- 2208-HÄNDELSER                   
004340*                                        INPUT                            
004350                                                                          
004360     SELECT W11126 ASSIGN W22210D2.                                       
004400     SKIP2                                                                
004500*--------------------------------------- PROGNOSUPPDATERINGS-             
004600*                                        TRANSAR                          
004700*                                        OUTPUT                           
004800                                                                          
004900     SELECT W22211 ASSIGN W22210D3.                                       
005000     SKIP2                                                                
005800*--------------------------------------- SORTERINGSFIL                    
005900                                                                          
006000     SELECT SRT ASSIGN W22210DS.                                          
006100     EJECT                                                                
006200 DATA DIVISION.                                                           
006300 FILE SECTION.                                                            
006400     SKIP2                                                                
006500 FD  W09257                                                               
006600     RECORDING V                                                          
006700     BLOCK 0.                                                             
006900                                                                          
007000*01  FILLER -COPY W222RP1 -L.                                             
007200     SKIP2                                                                
007300*01  FILLER -COPY W222RP2 -L.                                             
007500     SKIP2                                                                
007600*01  FILLER -COPY W222RP3 -L.                                             
007800     SKIP2                                                                
007900*01  FILLER -COPY W222RP4 -L.                                             
008100     SKIP2                                                                
008200*01  FILLER -COPY W222RP5 -L.                                             
008400     SKIP2                                                                
008500*01  FILLER -COPY W222RP6 -L.                                             
008700     SKIP2                                                                
008800*01  FILLER -COPY W222RP7 -L.                                             
009000     EJECT                                                                
009100 FD  W11126                                                               
009200     RECORDING F                                                          
009300     BLOCK 0.                                                             
009310                                                                          
009400*01  W11126-POST   -COPY W222RP1     -L.                                  
009510     EJECT                                                                
009520 FD  W22211                                                               
009530     RECORDING V                                                          
009540     BLOCK 0.                                                             
009550                                                                          
009600*01  POST -COPY W222RP1 -PRE U11RP1- -L.                                  
009800     SKIP2                                                                
009900*01  POST -COPY W222RP2 -PRE U11RP2- -L.                                  
010100     SKIP2                                                                
010200*01  POST -COPY W222RP3 -PRE U11RP3- -L.                                  
010400     SKIP2                                                                
010500*01  POST -COPY W222RP4 -PRE U11RP4- -L.                                  
010700     SKIP2                                                                
010800*01  POST -COPY W222RP5 -PRE U11RP5- -L.                                  
011000     SKIP2                                                                
011100*01  POST -COPY W222RP6 -PRE U11RP6- -L.                                  
011300                                                                          
011400*01  POST -COPY W222RP7 -PRE U11RP7- -L.                                  
011600     EJECT                                                                
013400 SD  SRT                                                                  
013500                .                                                         
013600                                                                          
013700*01  POST -COPY W222RP1 -PRE SRTRP1-.                                     
013900     SKIP2                                                                
014000*01  POST -COPY W222RP2 -PRE SRTRP2- -L.                                  
014200     SKIP2                                                                
014300*01  POST -COPY W222RP3 -PRE SRTRP3- -L.                                  
014500     SKIP2                                                                
014600*01  POST -COPY W222RP4 -PRE SRTRP4- -L.                                  
014800     SKIP2                                                                
014900*01  POST -COPY W222RP5 -PRE SRTRP5- -L.                                  
015100     SKIP2                                                                
015200*01  POST -COPY W222RP6 -PRE SRTRP6- -L.                                  
015400     SKIP2                                                                
015500*01  POST -COPY W222RP7 -PRE SRTRP7- -L.                                  
015700     EJECT                                                                
015800 WORKING-STORAGE SECTION.                                                 
015810*    -- CHECKED BY WY2000                                                 
015900     SKIP3                                                                
016300*                                                                         
016400     SKIP3                                                                
016500 01  RKOD                    PIC S9(4)               COMP SYNC.           
016600     SKIP3                                                                
016700 01  KONSTANTER.                                                          
016800     05  JA                  PIC X       VALUE 'J'.                       
016900     05  NEJ                 PIC X       VALUE 'N'.                       
017000     05  PROGRAM-NAMN        PIC X(6)    VALUE 'W22210'.                  
017100     SKIP3                                                                
017110 77  W11126-EOF-SW               PIC X       VALUE 'N'.                   
017120     88  END-OF-W11126                       VALUE 'J'.                   
017130     SKIP3                                                                
017200 01  INDEXFALT.                                                           
017300     05  XI                  PIC S9(4)               COMP SYNC.           
017400     SKIP3                                                                
017500 01  EOF-SWITCHAR.                                                        
017600     05  W09257-EOF          PIC X       VALUE 'N'.                       
017700     SKIP3                                                                
017800 01  DYNAMISKA-SUBPROGRAM.                                                
017900     05  POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
018100     EJECT                                                                
018200*--------------------------------------- PARAMETRAR TILL POSTSUM          
018300*                                                                         
018400*01  -COPY W0005 -PRE POSTSUM-                                            
018600     EJECT                                                                
018610*--------------------------------------- IN-AREA FIL W11126               
018620*                                        (HTYP 2208)                      
018630*01  AREA -COPY W222RP1 -PRE W11126-.                                     
018640     EJECT                                                                
018700*--------------------------------------- AREA FÖR PROGNOSUPP-             
018800*                                        DATERINGSTRANSAR                 
018900                                                                          
019000 01  RP-AREA                 PIC X(26).                                   
019100     SKIP3                                                                
019200*01  AREA -COPY W222RP1 -RED RP-AREA -PRE RP1-.                           
019400     SKIP3                                                                
019500*01  AREA -COPY W222RP2 -RED RP-AREA -PRE RP2-.                           
019700     SKIP3                                                                
019800*01  AREA -COPY W222RP3 -RED RP-AREA -PRE RP3-.                           
020000     SKIP3                                                                
020100*01  AREA -COPY W222RP4 -RED RP-AREA -PRE RP4-.                           
020300     SKIP3                                                                
020400*01  AREA -COPY W222RP5 -RED RP-AREA -PRE RP5-.                           
020600     SKIP3                                                                
020700*01  AREA -COPY W222RP6 -RED RP-AREA -PRE RP6-.                           
020900     SKIP3                                                                
021000*01  AREA -COPY W222RP7 -RED RP-AREA -PRE RP7-.                           
021200     EJECT                                                                
021300*--------------------------------------- AREA FÖR POSTER FRÅN             
021400*                                        HÄNDELSEREG KOD 2208             
021500*                                        OCH 2209                         
021600                                                                          
021700 01  RPT-AREA.                                                            
021800*    05  -COPY W092W001 -PRE RPT-                                         
022000     SKIP3                                                                
022100*    05  AREA -COPY W222RP3T -PRE RP3T-                                   
022300     SKIP3                                                                
022400*    05  AREA -COPY W222RP1T -RED RP3T-AREA -PRE RP1T-                    
023800     EJECT                                                                
023900 PROCEDURE DIVISION.                                                      
024000 MAIN SECTION.                                                            
024100     SKIP2                                                                
024600     SORT SRT ASCENDING SRTRP1-IDARTNR                                    
024700                        SRTRP1-KDCLAGER                                   
024800                        SRTRP1-IDPTYP                                     
024810          WITH DUPLICATES IN ORDER                                        
024900          INPUT PROCEDURE STYRDEL                                         
025000          GIVING W22211                                                   
025100                                                                          
025200     IF  SORT-RETURN > ZERO                                               
025300       DISPLAY '*** W22210 FEL VID SORTERING'                             
025400       MOVE 20 TO RKOD                                                    
025500     ELSE                                                                 
025600       MOVE ZERO TO RETURN-CODE                                           
025700       GOBACK                                                             
025800     END-IF                                                               
025900     .                                                                    
026000     EJECT                                                                
026100 STYRDEL SECTION.                                                         
026200     SKIP2                                                                
026300     PERFORM A-INITIERING                                                 
026400                                                                          
026500     PERFORM D-BEHANDLA-W09257                                            
026600                                                                          
026700     PERFORM B-BEHANDLA-KOD-2208                                          
026900     PERFORM Z-AVSLUTNING                                                 
027000     .                                                                    
027100     EJECT                                                                
027200******************************************************************        
027300*                                                                *        
027400*    INITIERING                                                  *        
027500*    ÖPPNA ALLA FILER                                            *        
027600*                                                                *        
027700******************************************************************        
027800                                                                          
027900 A-INITIERING SECTION.                                                    
028000                                                                          
028100     OPEN INPUT W09257                                                    
028110                W11126                                                    
028300                                                                          
028400     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
028500     .                                                                    
028600     EJECT                                                                
028700******************************************************************        
028800*                                                                *        
028900*    BEHANDLA KOD 2208                                           *        
029000*    LÄSER FIL W11126                                            *        
029200*                                                                *        
029300******************************************************************        
029400                                                                          
029500 B-BEHANDLA-KOD-2208 SECTION.                                             
029600                                                                          
029700     PERFORM S10-LAES-W11126                                              
030000                                                                          
030400     PERFORM UNTIL END-OF-W11126                                          
030600       MOVE W11126-AREA TO RP1-AREA                                       
030700       PERFORM S01-RELEASE-SRT                                            
030710                                                                          
030810                                                                          
030900       PERFORM S10-LAES-W11126                                            
031000     END-PERFORM                                                          
031200     .                                                                    
031300     EJECT                                                                
042100******************************************************************        
042200*                                                                *        
042300*    BEHANDLA W09257                                             *        
042400*    LÄMNAR SAMTLIGA POSTER PÅ INFILEN TILL SORTERING            *        
042500*                                                                *        
042600******************************************************************        
042700                                                                          
042800 D-BEHANDLA-W09257 SECTION.                                               
042900                                                                          
043000     PERFORM DA-LAS-W09257                                                
043100                                                                          
043200     PERFORM UNTIL                                                        
043300      NOT ( W09257-EOF = NEJ )                                            
043400       PERFORM S01-RELEASE-SRT                                            
043500       PERFORM DA-LAS-W09257                                              
043600     END-PERFORM                                                          
043700     .                                                                    
043800     EJECT                                                                
043900******************************************************************        
044000*                                                                *        
044100*    LÄS W22211 POST                                             *        
044200*                                                                *        
044300******************************************************************        
044400                                                                          
044500 DA-LAS-W09257 SECTION.                                                   
044600                                                                          
044700     READ W09257 INTO RP-AREA                                             
044800           AT END MOVE JA TO W09257-EOF                                   
044900     END-READ                                                             
045000                                                                          
045100     IF  W09257-EOF = NEJ                                                 
045200       MOVE 'W09257' TO POSTSUM-FDNAMN                                    
045300       MOVE 'W22210D1' TO POSTSUM-DDNAMN2                                 
045400       MOVE RP1-IDPTYP TO POSTSUM-TRANSTYP                                
045500       CALL POSTSUM USING POSTSUM-PARM                                    
045600     END-IF                                                               
045700     .                                                                    
045800     EJECT                                                                
045900******************************************************************        
046000*                                                                *        
046100*    AVSLUTNING                                                  *        
046200*    STÄNG ALLA FILER                                            *        
046300*    SKRIV UT POSTSUMS RÄKNEVERK                                 *        
046400*                                                                *        
046500******************************************************************        
046600                                                                          
046700 Z-AVSLUTNING SECTION.                                                    
046800                                                                          
046900     CLOSE W09257                                                         
046910           W11126                                                         
047100                                                                          
047200     MOVE 'S' TO POSTSUM-OPKOD                                            
047300     CALL POSTSUM USING POSTSUM-PARM                                      
047400     .                                                                    
047500     EJECT                                                                
047600******************************************************************        
047700*                                                                *        
047800*    RELEASE SRT                                                 *        
047900*    LÄMNA POST TILL SORTERING                                   *        
048000*                                                                *        
048100******************************************************************        
048200                                                                          
048300 S01-RELEASE-SRT SECTION.                                                 
048400                                                                          
048500     EVALUATE RP1-IDPTYP                                                  
048600     WHEN 'RP1'                                                           
048700       RELEASE SRTRP1-POST FROM RP1-AREA                                  
048800     WHEN 'RP2'                                                           
048900       RELEASE SRTRP2-POST FROM RP2-AREA                                  
049000     WHEN 'RP3'                                                           
049100       RELEASE SRTRP3-POST FROM RP3-AREA                                  
049200     WHEN 'RP4'                                                           
049300       RELEASE SRTRP4-POST FROM RP4-AREA                                  
049400     WHEN 'RP5'                                                           
049500       RELEASE SRTRP5-POST FROM RP5-AREA                                  
049600     WHEN 'RP6'                                                           
049700       RELEASE SRTRP6-POST FROM RP6-AREA                                  
049800     WHEN 'RP7'                                                           
049900       RELEASE SRTRP7-POST FROM RP7-AREA                                  
050000     END-EVALUATE                                                         
050100     MOVE 'W22211' TO POSTSUM-FDNAMN                                      
050200     MOVE 'W22210D3' TO POSTSUM-DDNAMN2                                   
050300     MOVE RP1-IDPTYP TO POSTSUM-TRANSTYP                                  
050400     CALL POSTSUM USING POSTSUM-PARM                                      
050500     .                                                                    
050600     EJECT                                                                
050700 S10-LAES-W11126 SECTION.                                                 
050800     SKIP2                                                                
050900     READ W11126 INTO W11126-AREA                                         
051000     AT END                                                               
051100        SET END-OF-W11126 TO TRUE                                         
051200                                                                          
051300     NOT AT END                                                           
051400        MOVE 'W11126' TO POSTSUM-FDNAMN                                   
051500        MOVE 'W22210D2' TO POSTSUM-DDNAMN2                                
051600        MOVE W11126-IDPTYP TO POSTSUM-TRANSTYP                            
051700        CALL POSTSUM USING POSTSUM-PARM                                   
051800     END-READ                                                             
051900     .                                                                    
