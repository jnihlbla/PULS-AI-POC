000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3304800.                                                 
000400 AUTHOR.        PETER DAHLÖF.                                             
000500 DATE-WRITTEN.  FEBRUARI 1990.                                            
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*    LÄSER FILEN W33047 SOM INNEHÅLLER A-URVAL + RESULTAT                 
001000*    MATCHAR MED W33031, SORTERAR OCH SKRIVER FILEN W33049                
001100*    OCH W33051                                                           
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400     SKIP2                                                                
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*    --- INFILER:                                                         
002000     SELECT W33031S                      ASSIGN TO W33048D1.              
002100     SELECT W33047S                      ASSIGN TO W33048D2.              
002200     SELECT SORTFIL                      ASSIGN TO W33048DS.              
002300*    --- UTFILER:                                                         
002400     SELECT W33049                       ASSIGN TO W33048D3.              
002500     SELECT W33051                       ASSIGN TO W33048D4.              
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W33031S                                                              
003200     LABEL RECORD   STANDARD                                              
003300     RECORDING      V                                                     
003400     BLOCK CONTAINS 0.                                                    
003500     SKIP2                                                                
003600*01  POST -COPY W3303103  -L -PRE I331-.                                  
003700*++INCLUDE W3303103C0                                                     
003800     SKIP2                                                                
003900*01  POST -COPY W3303102  -L -PRE I231-.                                  
004000*++INCLUDE W3303102C0                                                     
004100     SKIP2                                                                
004200*01  POST -COPY W3303104  -L -PRE I431-.                                  
004300*++INCLUDE W3303104C0                                                     
004400     EJECT                                                                
004500 FD  W33047S                                                              
004600     LABEL RECORD   STANDARD                                              
004700     RECORDING      F                                                     
004800     BLOCK CONTAINS 0.                                                    
004900     SKIP2                                                                
005000*01  POST -COPY W33041  -L -PRE I47-.                                     
005100*++INCLUDE W33041CCC0                                                     
005200     EJECT                                                                
005300 SD  SORTFIL                                                              
005400     RECORDING      V                                                     
005500     SKIP2                                                                
005600 01  SORTERAD-POST.                                                       
005700   03  SORT-IDUSER                 PIC X(8).                              
005800   03  SORT-DAREGDAT               PIC 9(8).                              
005900   03  SORT-TIREGTID               PIC 9(7).                              
006000   03  SORT-IDGTYP                 PIC 9(1).                              
006100   03  SORT-PRODSL                 PIC 9(3).                              
006200   03  SORT-ARTNR                  PIC 9(9).                              
006300   03  SORT-BEGREPP                PIC 9(5).                              
006400   03  CTEXT.                                                             
006500     05  FILLER                    PIC X(2608).                           
006600*03  POST -COPY W3303103  -L -PRE SRT331- -RED CTEXT.                     
006700*++INCLUDE W3303103C0                                                     
006800     SKIP2                                                                
006900*03  POST -COPY W3303102  -L -PRE SRT231- -RED CTEXT.                     
007000*++INCLUDE W3303102C0                                                     
007100     SKIP2                                                                
007200*03  POST -COPY W33041  -L -PRE SRT47- -RED CTEXT.                        
007300*++INCLUDE W33041CCC0                                                     
007400     EJECT                                                                
007500 FD  W33049                                                               
007600     LABEL RECORD   STANDARD                                              
007700     RECORDING      V                                                     
007800     BLOCK CONTAINS 0.                                                    
007900     SKIP2                                                                
008000*01  POST -COPY W3303103  -L -PRE U349-.                                  
008100*++INCLUDE W3303103C0                                                     
008200     SKIP2                                                                
008300*01  POST -COPY W3303102  -L -PRE U249-.                                  
008400*++INCLUDE W3303102C0                                                     
008500     SKIP2                                                                
008600*01  POST -COPY W33049  -L -PRE U049-.                                    
008700*++INCLUDE W33049CCC0                                                     
008800     EJECT                                                                
008900 FD  W33051                                                               
009000     LABEL RECORD   STANDARD                                              
009100     RECORDING      V                                                     
009200     BLOCK CONTAINS 0.                                                    
009300     SKIP2                                                                
009400*01  POST -COPY W3303103  -L -PRE U351-.                                  
009500*++INCLUDE W3303103C0                                                     
009600     SKIP2                                                                
009700*01  POST -COPY W3303102  -L -PRE U251-.                                  
009800*++INCLUDE W3303102C0                                                     
009900     SKIP2                                                                
010000*01  POST -COPY W33051  -L -PRE U051-.                                    
010100*++INCLUDE W33051CCC0                                                     
010200     EJECT                                                                
010300 WORKING-STORAGE SECTION.                                                 
010400     SKIP2                                                                
010401                                                                          
010410*    -- CHECKED BY WY2000                                                 
010500 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3304800'.            
010600 77  JA                          PIC X(1)    VALUE 'J'.                   
010700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
010800 77  I47-EOF                     PIC X(1)    VALUE 'N'.                   
010900 77  I31-EOF                     PIC X(1)    VALUE 'N'.                   
011000 77  SORT-EOF                    PIC X(1)    VALUE 'N'.                   
011100 77  IX                          PIC S9(4)   VALUE +1  COMP SYNC.         
011200 77  ABENDKOD                    PIC S9(4)   VALUE +16 COMP SYNC.         
011300 77  WS-RETOTBV-FRAAR            PIC S9(9)V9(2)  VALUE ZERO.              
011400 77  SPAR-BEGREPP                PIC 9(5)        VALUE ZERO.              
011500 77  SPAR-KDPRODSL               PIC 9(3)        VALUE ZERO.              
011600 77  SPAR-IDARTNR                PIC 9(9)        VALUE ZERO.              
011700 77  WS-SUTOTFSG-BUDG            PIC S9(11)V9(2) VALUE ZERO.              
011800 77  WS-DIFF                     PIC S9(9)V9(2)  VALUE ZERO.              
011900 77  WS-RETOTBV-RAAR-TG          PIC S9(9)V9(2)  VALUE ZERO.              
012000 77  WS-RETOTBV-RAAR-TB          PIC S9(9)V9(2)  VALUE ZERO.              
012100 77  WS-RETOTBV-RAAR             PIC S9(9)V9(2)  VALUE ZERO.              
012200 77  WS-RETOTBV-AAR              PIC S9(9)V9(2)  VALUE ZERO.              
012300 77  WS-RETOTBV-PER              PIC S9(9)V9(2)  VALUE ZERO.              
012400 77  WS-REFSG-AAR                PIC S9(9)V9(2)  VALUE ZERO.              
012500 77  WS-REFSG-RAAR               PIC S9(9)V9(2)  VALUE ZERO.              
012600 77  WS-RELEVANT-RAAR            PIC S9(9)V9(2)  VALUE ZERO.              
012700 77  WS-RELEVANT-AAR             PIC S9(9)V9(2)  VALUE ZERO.              
012800     SKIP2                                                                
012900 01  DYNAMISKA-SUBPROGRAM.                                                
013000     SKIP1                                                                
013100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
013200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013300     SKIP2                                                                
013400*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
013500*                                                                         
013600 01  FILLER                       PIC X(16)  VALUE 'POSTSUM'.             
013700*01  -COPY W0005 -PRE  POSTSUM-.                                          
013800*++INCLUDE W0005CCCC0                                                     
013900     EJECT                                                                
014000 01  FILLER                       PIC X(16)  VALUE 'I31-FILEN'.           
014100*                                                                         
014200*     FIL W33031                                                          
014300 01  I31-AREA.                                                            
014400   03  I31-IDUSER                     PIC X(8).                           
014500   03  I31-DAREGDAT                   PIC  9(8).                          
014600   03  I31-TIREGTID                   PIC S9(7) COMP-3.                   
014700   03  FILLER                         PIC X(8).                           
014800   03  I31-IDPTYP                     PIC X(3).                           
014900   03  I31-IDGTYP                     PIC S9(1) COMP-3.                   
015000   03  I31-IDTRANS                    PIC X(4).                           
015100   03  I31-IDKONCNR OCCURS 8 TIMES    PIC S9(3) COMP-3.                   
015200   03  I31-MARKNADS-GRP OCCURS 8 TIMES.                                   
015300     05  I31-KDMARK-BUDG-FOM          PIC S9(3) COMP-3.                   
015400     05  I31-KDMARK-BUDG-TOM          PIC S9(3) COMP-3.                   
015500   03  I31-DISTRIKT-GRP OCCURS 4 TIMES.                                   
015600     05  I31-IDDISTR-FOM              PIC S9(5) COMP-3.                   
015700     05  I31-IDDISTR-TOM              PIC S9(5) COMP-3.                   
015800   03  FILLER                         PIC X(2560).                        
015900*01  AREA  -COPY W3303103  -PRE I331- -RED I31-AREA.                      
016000*++INCLUDE W3303103C0                                                     
016100    EJECT                                                                 
016200*01  AREA  -COPY W3303102  -PRE I231- -RED I31-AREA.                      
016300*++INCLUDE W3303102C0                                                     
016400    EJECT                                                                 
016500 01  FILLER                       PIC X(16)  VALUE 'I47-FILEN'.           
016600*01  AREA  -COPY W33041  -PRE I47-.                                       
016700*++INCLUDE W33041CCC0                                                     
016800    EJECT                                                                 
016900 01  FILLER                       PIC X(16)  VALUE                        
017000                                          'WS-SORTERAD-AREA'.             
017100 01  WS-SORTERAD-AREA.                                                    
017200   03  WS-SORT-IDUSER              PIC X(8).                              
017300   03  WS-SORT-DAREGDAT            PIC 9(8).                              
017400   03  WS-SORT-TIREGTID            PIC 9(7).                              
017500   03  WS-SORT-GTYP                PIC 9(1).                              
017600   03  WS-SORT-PRODSL              PIC 9(3).                              
017700   03  WS-SORT-ARTNR               PIC 9(9).                              
017800   03  WS-SORT-BEGREPP             PIC 9(5).                              
017900   03  WS-CTEXT.                                                          
018000     05  FILLER                    PIC X(31).                             
018100     05  WS-SORT-IDGTYP            PIC S9(1) COMP-3.                      
018200     05  WS-SORT-IDTRANS           PIC X(4).                              
018300     05  FILLER                    PIC X(2608).                           
018400*03  AREA -COPY W3303103  -PRE SORT331- -RED WS-CTEXT.                    
018500*++INCLUDE W3303103C0                                                     
018600     SKIP2                                                                
018700*03  AREA -COPY W3303102  -PRE SORT231- -RED WS-CTEXT.                    
018800*++INCLUDE W3303102C0                                                     
018900     SKIP2                                                                
019000*03  AREA -COPY W33041  -PRE SORT47- -RED WS-CTEXT.                       
019100*++INCLUDE W33041CCC0                                                     
019200     EJECT                                                                
019300 01  FILLER                       PIC X(16)  VALUE 'UTFILER'.             
019400*                                                                         
019500*     FIL W33049                                                          
019600*01  AREA  -COPY W33049  -PRE U049-.                                      
019700*++INCLUDE W33049CCC0                                                     
019800     EJECT                                                                
019900*01  AREA  -COPY W33049  -PRE 49NTLZD-.                                   
020000*++INCLUDE W33049CCC0                                                     
020100     EJECT                                                                
020200 01  FILLER                       PIC X(16)  VALUE 'NIVÅ-MARKN'.          
020300*     FIL W33051                                                          
020400*01  AREA  -COPY W33051  -PRE U051-.                                      
020500*++INCLUDE W33051CCC0                                                     
020600     EJECT                                                                
020700*01  AREA  -COPY W33051  -PRE 51NTLZD-.                                   
020800*++INCLUDE W33051CCC0                                                     
020900     EJECT                                                                
021000 PROCEDURE DIVISION.                                                      
021100    SKIP2                                                                 
021200 STYR SECTION.                                                            
021300* BÖRJA MED ATT SORTERA 31 OCH 47 FILERNA                                 
021400     PERFORM A-INIT                                                       
021500     SORT SORTFIL                                                         
021600        ASCENDING KEY SORT-IDUSER                                         
021700                      SORT-DAREGDAT                                       
021800                      SORT-TIREGTID                                       
021900                      SORT-PRODSL                                         
022000                      SORT-ARTNR                                          
022100                      SORT-BEGREPP                                        
022200        INPUT PROCEDURE  B-PLOCKA-A-URVAL                                 
022300        OUTPUT PROCEDURE C-BEHANDLA-OCH-SKRIV                             
022400     IF SORT-RETURN = ZERO                                                
022500        PERFORM Z-FINIT                                                   
022600        MOVE ZERO TO RETURN-CODE                                          
022700        GOBACK                                                            
022800     ELSE                                                                 
022900        DISPLAY 'FEL I SORTERINGEN'                                       
023000        CALL ABEND USING ABENDKOD                                         
023100     END-IF                                                               
023200     .                                                                    
023300     EJECT                                                                
023400 A-INIT SECTION.                                                          
023500     SKIP2                                                                
023600     OPEN INPUT  W33031S                                                  
023700                 W33047S                                                  
023800     OPEN OUTPUT W33049                                                   
023900                 W33051                                                   
024000     MOVE PROGRAM-NAMN               TO POSTSUM-PROGNAMN                  
024100     INITIALIZE 49NTLZD-AREA                                              
024200                51NTLZD-AREA                                              
024300     MOVE 49NTLZD-AREA                      TO U049-AREA                  
024400     MOVE 51NTLZD-AREA                      TO U051-AREA                  
024500     .                                                                    
024600     EJECT                                                                
024700 B-PLOCKA-A-URVAL    SECTION.                                             
024800     SKIP2                                                                
024900     PERFORM S01-LAS-31-FIL                                               
025000     PERFORM S02-LAS-47-FIL                                               
025100     PERFORM UNTIL I47-EOF = JA AND                                       
025200     I31-EOF = JA                                                         
025300        IF I47-EOF = NEJ                                                  
025400           IF I47-IDUSER   = I31-IDUSER   AND                             
025500           I47-DAREGDAT    = I31-DAREGDAT AND                             
025600           I47-TIREGTID    = I31-TIREGTID                                 
025700              IF  I47-IDPTYP   = 'A1 '                                    
025800                 PERFORM S09-SKRIV-49-FIL-I31-AREA                        
025900                 PERFORM S08-INITIERA-U049                                
026000                 PERFORM BA-SMMR-RTKLR-SKRV-49-FL                         
026100              ELSE                                                        
026200                 PERFORM BB-RELEASE                                       
026300              END-IF                                                      
026400           ELSE                                                           
026500              IF I31-IDPTYP = 'A1 '                                       
026600                 MOVE ZERO                   TO I31-IDGTYP                
026700                 PERFORM S09-SKRIV-49-FIL-I31-AREA                        
026800              ELSE                                                        
026900                 IF I31-IDPTYP = 'A2 '                                    
027000                    MOVE ZERO                TO I31-IDGTYP                
027100                    PERFORM S11-RELEASE-URVALSPOST                        
027200                 END-IF                                                   
027300              END-IF                                                      
027400           END-IF                                                         
027500        ELSE                                                              
027600           IF I31-IDPTYP = 'A1 '                                          
027700              MOVE ZERO                      TO I31-IDGTYP                
027800              PERFORM S09-SKRIV-49-FIL-I31-AREA                           
027900           ELSE                                                           
028000              IF I31-IDPTYP = 'A2 '                                       
028100                 MOVE ZERO                   TO I31-IDGTYP                
028200                 PERFORM S11-RELEASE-URVALSPOST                           
028300              END-IF                                                      
028400           END-IF                                                         
028500        END-IF                                                            
028600        PERFORM S01-LAS-31-FIL                                            
028700     END-PERFORM                                                          
028800     .                                                                    
028900     EJECT                                                                
029000 BA-SMMR-RTKLR-SKRV-49-FL  SECTION.                                       
029100     SKIP2                                                                
029200     MOVE I47-IDARTNR                  TO SPAR-IDARTNR                    
029300     PERFORM UNTIL I47-EOF = JA OR                                        
029400     I47-IDUSER    NOT  = I31-IDUSER   OR                                 
029500     I47-DAREGDAT  NOT  = I31-DAREGDAT OR                                 
029600     I47-TIREGTID  NOT  = I31-TIREGTID                                    
029700        IF I47-IDARTNR = SPAR-IDARTNR                                     
029800           CONTINUE                                                       
029900        ELSE                                                              
030000           PERFORM S10-SKRIV-49-FIL-NTLZE                                 
030100           MOVE I47-IDARTNR            TO SPAR-IDARTNR                    
030200        END-IF                                                            
030300        PERFORM BAA-ADDERA                                                
030400        PERFORM S02-LAS-47-FIL                                            
030500     END-PERFORM                                                          
030600     PERFORM S10-SKRIV-49-FIL-NTLZE                                       
030700     .                                                                    
030800     EJECT                                                                
030900 BAA-ADDERA         SECTION.                                              
031000     SKIP2                                                                
031100     ADD  I47-SUARTFSG-PER       TO U049-SUARTFSG-PER                     
031200     ADD  I47-SUARTFSG-AAR       TO U049-SUARTFSG-AAR                     
031300     ADD  I47-SUARTFSG-FAAR      TO U049-SUARTFSG-FAAR                    
031400     ADD  I47-SUARTFSG-RAAR      TO U049-SUARTFSG-RAAR                    
031500     ADD  I47-SUARTFSG-FRAAR     TO U049-SUARTFSG-FRAAR                   
031600     ADD  I47-SULEVANT-PER       TO U049-SULEVANT-PER                     
031700     ADD  I47-SULEVANT-AAR       TO U049-SULEVANT-AAR                     
031800     ADD  I47-SULEVANT-FAAR      TO U049-SULEVANT-FAAR                    
031900     ADD  I47-SULEVANT-RAAR      TO U049-SULEVANT-RAAR                    
032000     ADD  I47-SULEVANT-FRAAR     TO U049-SULEVANT-FRAAR                   
032100     ADD  I47-SUARTSJK-PER       TO U049-SUARTSJK-PER                     
032200     ADD  I47-SUARTSJK-AAR       TO U049-SUARTSJK-AAR                     
032300     ADD  I47-SUARTSJK-FAAR      TO U049-SUARTSJK-FAAR                    
032400     ADD  I47-SUARTSJK-RAAR      TO U049-SUARTSJK-RAAR                    
032500     ADD  I47-SUARTSJK-FRAAR     TO U049-SUARTSJK-FRAAR                   
032600     .                                                                    
032700     EJECT                                                                
032800 BB-RELEASE             SECTION.                                          
032900     SKIP2                                                                
033000     MOVE +1                         TO IX                                
033100     IF I31-IDGTYP = +1                                                   
033200        PERFORM BBA-SORTERA-PA-DISTR                                      
033300     ELSE                                                                 
033400        IF I31-IDGTYP = +2                                                
033500           PERFORM BBB-SORTERA-PA-KONCNR                                  
033600        ELSE                                                              
033700           IF I31-IDGTYP = +3                                             
033800              PERFORM BBC-SORTERA-PA-MARKN                                
033900           ELSE                                                           
034000              IF I31-IDGTYP = +4                                          
034100                 PERFORM BBD-SORTERA-PA-WORLD-WIDE                        
034200              END-IF                                                      
034300           END-IF                                                         
034400        END-IF                                                            
034500     END-IF                                                               
034600     .                                                                    
034700     EJECT                                                                
034800 BBA-SORTERA-PA-DISTR       SECTION.                                      
034900     SKIP2                                                                
035000     PERFORM UNTIL                                                        
035100     I47-IDUSER   NOT = I31-IDUSER   OR                                   
035200     I47-DAREGDAT NOT = I31-DAREGDAT OR                                   
035300     I47-TIREGTID NOT = I31-TIREGTID OR                                   
035400     I47-EOF = JA                                                         
035500        MOVE I47-IDUSER               TO WS-SORT-IDUSER                   
035600        MOVE I47-DAREGDAT             TO WS-SORT-DAREGDAT                 
035700        MOVE I47-TIREGTID             TO WS-SORT-TIREGTID                 
035800        MOVE I47-IDGTYP               TO WS-SORT-GTYP                     
035900        MOVE I47-KDPRODSL             TO WS-SORT-PRODSL                   
036000        MOVE I47-IDARTNR              TO WS-SORT-ARTNR                    
036100        MOVE I47-IDDISTR              TO WS-SORT-BEGREPP                  
036200        MOVE I47-AREA                 TO SORT47-AREA                      
036300        RELEASE SORTERAD-POST FROM WS-SORTERAD-AREA                       
036400        PERFORM S02-LAS-47-FIL                                            
036500     END-PERFORM                                                          
036600     PERFORM S11-RELEASE-URVALSPOST                                       
036700     .                                                                    
036800     EJECT                                                                
036900 BBB-SORTERA-PA-KONCNR      SECTION.                                      
037000     SKIP2                                                                
037100     PERFORM UNTIL                                                        
037200     I47-IDUSER   NOT = I31-IDUSER   OR                                   
037300     I47-DAREGDAT NOT = I31-DAREGDAT OR                                   
037400     I47-TIREGTID NOT = I31-TIREGTID OR                                   
037500     I47-EOF = JA                                                         
037600        MOVE I47-IDUSER               TO WS-SORT-IDUSER                   
037700        MOVE I47-DAREGDAT             TO WS-SORT-DAREGDAT                 
037800        MOVE I47-TIREGTID             TO WS-SORT-TIREGTID                 
037900        MOVE I47-IDGTYP               TO WS-SORT-GTYP                     
038000        MOVE I47-KDPRODSL             TO WS-SORT-PRODSL                   
038100        MOVE I47-IDARTNR              TO WS-SORT-ARTNR                    
038200        MOVE I47-IDKONCNR             TO WS-SORT-BEGREPP                  
038300        MOVE I47-AREA                 TO SORT47-AREA                      
038400        RELEASE SORTERAD-POST FROM WS-SORTERAD-AREA                       
038500        PERFORM S02-LAS-47-FIL                                            
038600     END-PERFORM                                                          
038700     PERFORM S11-RELEASE-URVALSPOST                                       
038800     .                                                                    
038900     EJECT                                                                
039000 BBC-SORTERA-PA-MARKN       SECTION.                                      
039100     SKIP2                                                                
039200     PERFORM UNTIL                                                        
039300     I47-IDUSER   NOT = I31-IDUSER   OR                                   
039400     I47-DAREGDAT NOT = I31-DAREGDAT OR                                   
039500     I47-TIREGTID NOT = I31-TIREGTID OR                                   
039600     I47-EOF = JA                                                         
039700        MOVE I47-IDUSER               TO WS-SORT-IDUSER                   
039800        MOVE I47-DAREGDAT             TO WS-SORT-DAREGDAT                 
039900        MOVE I47-TIREGTID             TO WS-SORT-TIREGTID                 
040000        MOVE I47-IDGTYP               TO WS-SORT-GTYP                     
040100        MOVE I47-KDPRODSL             TO WS-SORT-PRODSL                   
040200        MOVE I47-IDARTNR              TO WS-SORT-ARTNR                    
040300        MOVE I47-KDMARK-BUDG          TO WS-SORT-BEGREPP                  
040400        MOVE I47-AREA                 TO SORT47-AREA                      
040500        RELEASE SORTERAD-POST FROM WS-SORTERAD-AREA                       
040600        PERFORM S02-LAS-47-FIL                                            
040700     END-PERFORM                                                          
040800     PERFORM S11-RELEASE-URVALSPOST                                       
040900     .                                                                    
041000     EJECT                                                                
041100 BBD-SORTERA-PA-WORLD-WIDE  SECTION.                                      
041200     SKIP2                                                                
041300     PERFORM UNTIL                                                        
041400     I47-IDUSER   NOT = I31-IDUSER   OR                                   
041500     I47-DAREGDAT NOT = I31-DAREGDAT OR                                   
041600     I47-TIREGTID NOT = I31-TIREGTID OR                                   
041700     I47-EOF = JA                                                         
041800        MOVE I47-IDUSER               TO WS-SORT-IDUSER                   
041900        MOVE I47-DAREGDAT             TO WS-SORT-DAREGDAT                 
042000        MOVE I47-TIREGTID             TO WS-SORT-TIREGTID                 
042100        MOVE I47-IDGTYP               TO WS-SORT-GTYP                     
042200        MOVE I47-KDPRODSL             TO WS-SORT-PRODSL                   
042300        MOVE I47-IDARTNR              TO WS-SORT-ARTNR                    
042400        MOVE I47-KDMARK-BUDG          TO WS-SORT-BEGREPP                  
042500        MOVE I47-AREA                 TO SORT47-AREA                      
042600        RELEASE SORTERAD-POST FROM WS-SORTERAD-AREA                       
042700        PERFORM S02-LAS-47-FIL                                            
042800     END-PERFORM                                                          
042900     PERFORM S11-RELEASE-URVALSPOST                                       
043000     .                                                                    
043100     EJECT                                                                
043200 C-BEHANDLA-OCH-SKRIV     SECTION.                                        
043300     SKIP2                                                                
043400     PERFORM S03-RETURN                                                   
043500     PERFORM UNTIL SORT-EOF = JA                                          
043600        IF WS-SORT-IDGTYP < +5                                            
043700           PERFORM S12-SKRIV-51-FIL-URVAL                                 
043800           PERFORM S03-RETURN                                             
043900        ELSE                                                              
044000           PERFORM S06-INITIERA-U051-SPAR                                 
044100           MOVE WS-SORT-PRODSL              TO SPAR-KDPRODSL              
044200           MOVE WS-SORT-ARTNR               TO SPAR-IDARTNR               
044300           MOVE WS-SORT-BEGREPP             TO SPAR-BEGREPP               
044400           PERFORM UNTIL WS-SORT-IDGTYP < 5 OR                            
044500           SORT-EOF = JA                                                  
044600              PERFORM CA-TESTA-NIVA-SUMMERA                               
044700              PERFORM CB-ADDERA                                           
044800              PERFORM S03-RETURN                                          
044900           END-PERFORM                                                    
045000           PERFORM S05-BERAKNA-SKRIV-BEGREPP                              
045100        END-IF                                                            
045200     END-PERFORM                                                          
045300     .                                                                    
045400     EJECT                                                                
045500 CA-TESTA-NIVA-SUMMERA   SECTION.                                         
045600     SKIP2                                                                
045700     IF SPAR-KDPRODSL = WS-SORT-PRODSL                                    
045800        IF SPAR-IDARTNR = SORT47-IDARTNR                                  
045900           IF SPAR-BEGREPP  = WS-SORT-BEGREPP                             
046000              CONTINUE                                                    
046100           ELSE                                                           
046200              PERFORM S05-BERAKNA-SKRIV-BEGREPP                           
046300              MOVE WS-SORT-BEGREPP TO SPAR-BEGREPP                        
046400           END-IF                                                         
046500        ELSE                                                              
046600           PERFORM S05-BERAKNA-SKRIV-BEGREPP                              
046700           MOVE WS-SORT-BEGREPP    TO SPAR-BEGREPP                        
046800           MOVE SORT47-IDARTNR     TO SPAR-IDARTNR                        
046900        END-IF                                                            
047000     ELSE                                                                 
047100        PERFORM S05-BERAKNA-SKRIV-BEGREPP                                 
047200        MOVE WS-SORT-BEGREPP       TO SPAR-BEGREPP                        
047300        MOVE SORT47-IDARTNR        TO SPAR-IDARTNR                        
047400        MOVE WS-SORT-PRODSL        TO SPAR-KDPRODSL                       
047500     END-IF                                                               
047600     .                                                                    
047700     EJECT                                                                
047800 CB-ADDERA                  SECTION.                                      
047900     SKIP2                                                                
048000     ADD  SORT47-SUARTFSG-PER    TO U051-SUARTFSG-PER                     
048100     ADD  SORT47-SUARTFSG-AAR    TO U051-SUARTFSG-AAR                     
048200     ADD  SORT47-SUARTFSG-FAAR   TO U051-SUARTFSG-FAAR                    
048300     ADD  SORT47-SUARTFSG-RAAR   TO U051-SUARTFSG-RAAR                    
048400     ADD  SORT47-SUARTFSG-FRAAR  TO U051-SUARTFSG-FRAAR                   
048500     ADD  SORT47-SULEVANT-PER    TO U051-SULEVANT-PER                     
048600     ADD  SORT47-SULEVANT-AAR    TO U051-SULEVANT-AAR                     
048700     ADD  SORT47-SULEVANT-FAAR   TO U051-SULEVANT-FAAR                    
048800     ADD  SORT47-SULEVANT-RAAR   TO U051-SULEVANT-RAAR                    
048900     ADD  SORT47-SULEVANT-FRAAR  TO U051-SULEVANT-FRAAR                   
049000     ADD  SORT47-SUARTSJK-PER    TO U051-SUARTSJK-PER                     
049100     ADD  SORT47-SUARTSJK-AAR    TO U051-SUARTSJK-AAR                     
049200     ADD  SORT47-SUARTSJK-FAAR   TO U051-SUARTSJK-FAAR                    
049300     ADD  SORT47-SUARTSJK-RAAR   TO U051-SUARTSJK-RAAR                    
049400     ADD  SORT47-SUARTSJK-FRAAR  TO U051-SUARTSJK-FRAAR                   
049500     .                                                                    
049600     EJECT                                                                
049700 S01-LAS-31-FIL    SECTION.                                               
049800     SKIP2                                                                
049900     READ W33031S INTO I31-AREA                                           
050000     AT END                                                               
050100       MOVE JA                    TO I31-EOF                              
050200     END-READ                                                             
050300                                                                          
050400     IF I31-EOF = NEJ                                                     
050500       MOVE '    '                TO POSTSUM-TRANSTYP                     
050600       MOVE 'W33031'              TO POSTSUM-FDNAMN                       
050700       MOVE 'W33048D1'            TO POSTSUM-DDNAMN2                      
050800       CALL POSTSUM USING POSTSUM-PARM                                    
050900     END-IF                                                               
051000     .                                                                    
051100     EJECT                                                                
051200 S02-LAS-47-FIL   SECTION.                                                
051300     SKIP2                                                                
051400     READ W33047S INTO I47-AREA                                           
051500     AT END                                                               
051600       MOVE JA                    TO I47-EOF                              
051700       MOVE +99999999             TO I47-DAREGDAT                         
051710       MOVE +999999               TO                                      
051800                                     I47-TIREGTID                         
051900     END-READ                                                             
052000                                                                          
052100     IF I47-EOF = NEJ                                                     
052200       MOVE '    '                TO POSTSUM-TRANSTYP                     
052300       MOVE 'W33047'              TO POSTSUM-FDNAMN                       
052400       MOVE 'W33048D2'            TO POSTSUM-DDNAMN2                      
052500       CALL POSTSUM USING POSTSUM-PARM                                    
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 S03-RETURN       SECTION.                                                
053000     SKIP2                                                                
053100     RETURN SORTFIL INTO WS-SORTERAD-AREA                                 
053200     AT END                                                               
053300       MOVE JA                    TO SORT-EOF                             
053400     END-RETURN                                                           
053500                                                                          
053600     IF SORT-EOF = NEJ                                                    
053700       MOVE 'SORT'                TO POSTSUM-TRANSTYP                     
053800       MOVE '      '              TO POSTSUM-FDNAMN                       
053900       MOVE 'W33048DS'            TO POSTSUM-DDNAMN2                      
054000       CALL POSTSUM USING POSTSUM-PARM                                    
054100     END-IF                                                               
054200     .                                                                    
054300     EJECT                                                                
054400 S04-SKRIV-49-FIL         SECTION.                                        
054500     SKIP2                                                                
054600     WRITE U049-POST FROM U049-AREA                                       
054700     MOVE '    '                 TO POSTSUM-TRANSTYP                      
054800     MOVE 'W33049'               TO POSTSUM-FDNAMN                        
054900     MOVE 'W33048D3'             TO POSTSUM-DDNAMN2                       
055000     CALL POSTSUM USING POSTSUM-PARM                                      
055100     .                                                                    
055200     EJECT                                                                
055300 S05-BERAKNA-SKRIV-BEGREPP     SECTION.                                   
055400     SKIP2                                                                
055500*    AF2 TB           ***                                                 
055600     COMPUTE U051-SUTOTBV-RAAR ROUNDED = U051-SUARTFSG-RAAR -             
055700                                 U051-SUARTSJK-RAAR                       
055800***  AF5 TB           ***                                                 
055900     COMPUTE U051-SUTOTBV-PER ROUNDED = U051-SUARTFSG-PER -               
056000                                 U051-SUARTSJK-PER                        
056100*    AF2/AF1 TB       ***                                                 
056200     COMPUTE WS-DIFF = U051-SUARTFSG-FRAAR -                              
056300                       U051-SUARTSJK-FRAAR                                
056400     IF WS-DIFF = ZERO                                                    
056500        CONTINUE                                                          
056600     ELSE                                                                 
056700        COMPUTE WS-RETOTBV-RAAR-TB ROUNDED =                              
056800        (100 * (U051-SUTOTBV-RAAR - WS-DIFF)) / WS-DIFF                   
056900        IF WS-RETOTBV-RAAR-TB > +99.9                                     
057000           MOVE +99.9              TO                                     
057100                                U051-RETOTBV-RAAR-TB                      
057200        ELSE                                                              
057300           IF WS-RETOTBV-RAAR-TB < -99.9                                  
057400              MOVE -99.9           TO                                     
057500                                U051-RETOTBV-RAAR-TB                      
057600           ELSE                                                           
057700              MOVE WS-RETOTBV-RAAR-TB  TO                                 
057800                                U051-RETOTBV-RAAR-TB                      
057900           END-IF                                                         
058000        END-IF                                                            
058100     END-IF                                                               
058200***                  TÄCKNINGSGRAD    ***                                 
058300***  AF2 TG           ***                                                 
058400     IF U051-SUARTFSG-RAAR = ZERO                                         
058500        CONTINUE                                                          
058600     ELSE                                                                 
058700        COMPUTE WS-RETOTBV-RAAR ROUNDED =                                 
058800        (100 * (U051-SUARTFSG-RAAR -                                      
058900        U051-SUARTSJK-RAAR)) / U051-SUARTFSG-RAAR                         
059000        IF WS-RETOTBV-RAAR > +99.9                                        
059100           MOVE +99.9              TO                                     
059200                                U051-RETOTBV-RAAR                         
059300        ELSE                                                              
059400           IF WS-RETOTBV-RAAR < -99.9                                     
059500              MOVE -99.9           TO                                     
059600                                U051-RETOTBV-RAAR                         
059700           ELSE                                                           
059800              MOVE WS-RETOTBV-RAAR    TO                                  
059900                                   U051-RETOTBV-RAAR                      
060000           END-IF                                                         
060100        END-IF                                                            
060200     END-IF                                                               
060300***  AF4 TG           ***                                                 
060400     IF U051-SUARTFSG-AAR = ZERO                                          
060500        CONTINUE                                                          
060600     ELSE                                                                 
060700        COMPUTE WS-RETOTBV-AAR ROUNDED =                                  
060800        (100 * (U051-SUARTFSG-AAR -                                       
060900        U051-SUARTSJK-AAR)) / U051-SUARTFSG-AAR                           
061000        IF WS-RETOTBV-AAR > +99.9                                         
061100           MOVE +99.9              TO                                     
061200                                U051-RETOTBV-AAR                          
061300        ELSE                                                              
061400           IF WS-RETOTBV-AAR < -99.9                                      
061500              MOVE -99.9              TO                                  
061600                                U051-RETOTBV-AAR                          
061700           ELSE                                                           
061800              MOVE WS-RETOTBV-AAR     TO                                  
061900                                   U051-RETOTBV-AAR                       
062000           END-IF                                                         
062100        END-IF                                                            
062200     END-IF                                                               
062300***  AF5 TG           ***                                                 
062400     IF U051-SUARTFSG-PER = ZERO                                          
062500        CONTINUE                                                          
062600     ELSE                                                                 
062700        COMPUTE WS-RETOTBV-PER ROUNDED =                                  
062800        (100 * (U051-SUARTFSG-PER -                                       
062900        U051-SUARTSJK-PER)) / U051-SUARTFSG-PER                           
063000        IF WS-RETOTBV-PER > +99.9                                         
063100           MOVE +99.9              TO                                     
063200                                U051-RETOTBV-PER                          
063300        ELSE                                                              
063400           IF WS-RETOTBV-PER < -99.9                                      
063500              MOVE -99.9              TO                                  
063600                                U051-RETOTBV-PER                          
063700           ELSE                                                           
063800              MOVE WS-RETOTBV-PER     TO                                  
063900                                   U051-RETOTBV-PER                       
064000           END-IF                                                         
064100        END-IF                                                            
064200     END-IF                                                               
064300***  AF1 TG           ***                                                 
064400     SKIP2                                                                
064500     IF U051-SUARTFSG-FRAAR = ZERO                                        
064600        CONTINUE                                                          
064700     ELSE                                                                 
064800        COMPUTE WS-RETOTBV-FRAAR = (100 * (U051-SUARTFSG-FRAAR -          
064900        U051-SUARTSJK-FRAAR)) / U051-SUARTFSG-FRAAR                       
065000     END-IF                                                               
065100***  AF2/AF1 TG       ***                                                 
065200     COMPUTE WS-RETOTBV-RAAR-TG ROUNDED =                                 
065300     WS-RETOTBV-RAAR - WS-RETOTBV-FRAAR                                   
065400     IF WS-RETOTBV-RAAR-TG > +99.9                                        
065500        MOVE +99.9              TO                                        
065600                                U051-RETOTBV-RAAR-TG                      
065700     ELSE                                                                 
065800        IF WS-RETOTBV-RAAR-TG < -99.9                                     
065900           MOVE -99.9              TO                                     
066000                                U051-RETOTBV-RAAR-TG                      
066100        ELSE                                                              
066200           MOVE WS-RETOTBV-RAAR-TG TO                                     
066300                                   U051-RETOTBV-RAAR-TG                   
066400        END-IF                                                            
066500     END-IF                                                               
066600*************************     AVVIKELSE        ***                        
066700***  AVV SEK AF4/AF3                                                      
066800     IF U051-SUARTFSG-FAAR = ZERO                                         
066900        CONTINUE                                                          
067000     ELSE                                                                 
067100        COMPUTE WS-REFSG-AAR ROUNDED = (100 *                             
067200        (U051-SUARTFSG-AAR - U051-SUARTFSG-FAAR)) /                       
067300                               U051-SUARTFSG-FAAR                         
067400        IF WS-REFSG-AAR   > +99.9                                         
067500           MOVE +99.9              TO                                     
067600                                U051-REFSG-AAR                            
067700        ELSE                                                              
067800           IF WS-REFSG-AAR   < -99.9                                      
067900              MOVE -99.9              TO                                  
068000                                U051-REFSG-AAR                            
068100           ELSE                                                           
068200              MOVE WS-REFSG-AAR       TO                                  
068300                                U051-REFSG-AAR                            
068400           END-IF                                                         
068500        END-IF                                                            
068600     END-IF                                                               
068700***  AVV SEK AF2/AF1                                                      
068800     IF U051-SUARTFSG-FRAAR = ZERO                                        
068900        CONTINUE                                                          
069000     ELSE                                                                 
069100        COMPUTE WS-REFSG-RAAR ROUNDED = (100 *                            
069200        (U051-SUARTFSG-RAAR - U051-SUARTFSG-FRAAR)) /                     
069300                                U051-SUARTFSG-FRAAR                       
069400        IF WS-REFSG-RAAR  > +99.9                                         
069500           MOVE +99.9              TO                                     
069600                                U051-REFSG-RAAR                           
069700        ELSE                                                              
069800           IF WS-REFSG-RAAR  < -99.9                                      
069900              MOVE -99.9              TO                                  
070000                                U051-REFSG-RAAR                           
070100           ELSE                                                           
070200              MOVE WS-REFSG-RAAR      TO                                  
070300                                U051-REFSG-RAAR                           
070400           END-IF                                                         
070500        END-IF                                                            
070600     END-IF                                                               
070700***  AVV STYCK AF2/AF1                                                    
070800     IF U051-SULEVANT-FRAAR = ZERO                                        
070900        CONTINUE                                                          
071000     ELSE                                                                 
071100        COMPUTE WS-RELEVANT-RAAR ROUNDED = (100 *                         
071200        (U051-SULEVANT-RAAR - U051-SULEVANT-FRAAR)) /                     
071300                               U051-SULEVANT-FRAAR                        
071400        IF WS-RELEVANT-RAAR > +99.9                                       
071500           MOVE +99.9              TO                                     
071600                                U051-RELEVANT-RAAR                        
071700        ELSE                                                              
071800           IF WS-RELEVANT-RAAR < -99.9                                    
071900              MOVE -99.9              TO                                  
072000                                U051-RELEVANT-RAAR                        
072100           ELSE                                                           
072200              MOVE WS-RELEVANT-RAAR   TO                                  
072300                                U051-RELEVANT-RAAR                        
072400           END-IF                                                         
072500        END-IF                                                            
072600     END-IF                                                               
072700     PERFORM S07-SKRIV-51-FIL-RESULTAT                                    
072800     IF SORT-EOF = NEJ                                                    
072900        PERFORM S06-INITIERA-U051-SPAR                                    
073000     END-IF                                                               
073100     .                                                                    
073200    EJECT                                                                 
073300 S06-INITIERA-U051-SPAR     SECTION.                                      
073400     SKIP2                                                                
073500     MOVE 51NTLZD-AREA                   TO U051-AREA                     
073600     IF WS-SORT-IDGTYP = 5                                                
073700        MOVE SORT47-001-GRUPP            TO U051-001-GRUPP                
073800        MOVE SORT47-IDARTNR              TO U051-IDARTNR                  
073900        MOVE SORT47-BEART-SVE            TO U051-BEART-SVE                
074000        MOVE SORT47-KDPRODSL             TO U051-KDPRODSL                 
074100        MOVE SORT47-BEPRODSL             TO U051-BEPRODSL                 
074200        MOVE SORT47-IDDISTR              TO U051-IDDISTR                  
074300        MOVE SORT47-IDKONCNR             TO U051-IDKONCNR                 
074400        MOVE SORT47-KDMARK-BUDG          TO U051-KDMARK-BUDG              
074500        MOVE SORT47-BEMARK-BUDG          TO U051-BEMARK-BUDG              
074600     END-IF                                                               
074700     .                                                                    
074800     EJECT                                                                
074900 S07-SKRIV-51-FIL-RESULTAT   SECTION.                                     
075000     SKIP2                                                                
075100     WRITE U051-POST FROM U051-AREA                                       
075200     MOVE '    '                 TO POSTSUM-TRANSTYP                      
075300     MOVE 'W33051'               TO POSTSUM-FDNAMN                        
075400     MOVE 'W33048D4'             TO POSTSUM-DDNAMN2                       
075500     CALL POSTSUM USING POSTSUM-PARM                                      
075600     .                                                                    
075700     EJECT                                                                
075800 S08-INITIERA-U049          SECTION.                                      
075900     SKIP2                                                                
076000     MOVE 49NTLZD-AREA                      TO U049-AREA                  
076100     IF I47-EOF = NEJ                                                     
076200        MOVE I47-001-GRUPP                  TO U049-001-GRUPP             
076300        MOVE I47-IDARTNR                    TO U049-IDARTNR               
076400        MOVE I47-BEART-SVE                  TO U049-BEART-SVE             
076500        MOVE I47-KDPRODSL                   TO U049-KDPRODSL              
076600        MOVE I47-BEPRODSL                   TO U049-BEPRODSL              
076700     END-IF                                                               
076800     .                                                                    
076900     EJECT                                                                
077000 S09-SKRIV-49-FIL-I31-AREA  SECTION.                                      
077100     SKIP2                                                                
077200     IF I31-IDTRANS = '3202'                                              
077300        WRITE U249-POST FROM I231-AREA                                    
077400     ELSE                                                                 
077500        WRITE U349-POST FROM I331-AREA                                    
077600     END-IF                                                               
077700     MOVE '    '                 TO POSTSUM-TRANSTYP                      
077800     MOVE 'W33049'               TO POSTSUM-FDNAMN                        
077900     MOVE 'W33048D3'             TO POSTSUM-DDNAMN2                       
078000     CALL POSTSUM USING POSTSUM-PARM                                      
078100     .                                                                    
078200     EJECT                                                                
078300 S10-SKRIV-49-FIL-NTLZE    SECTION.                                       
078400     SKIP2                                                                
078500*    AF2 TB           ***                                                 
078600     COMPUTE U049-SUTOTBV-RAAR = U049-SUARTFSG-RAAR -                     
078700                                 U049-SUARTSJK-RAAR                       
078800***  AF1 TG           ***                                                 
078900     SKIP2                                                                
079000     IF U049-SUARTFSG-FRAAR = ZERO                                        
079100        CONTINUE                                                          
079200     ELSE                                                                 
079300        COMPUTE WS-RETOTBV-FRAAR = (100 * (U049-SUARTFSG-FRAAR -          
079400        U049-SUARTSJK-FRAAR)) / U049-SUARTFSG-FRAAR                       
079500     END-IF                                                               
079600***  AF2 TG           ***                                                 
079700     SKIP2                                                                
079800     IF U049-SUARTFSG-RAAR = ZERO                                         
079900        CONTINUE                                                          
080000     ELSE                                                                 
080100        COMPUTE WS-RETOTBV-RAAR = (100 * (U049-SUARTFSG-RAAR -            
080200        U049-SUARTSJK-RAAR)) / U049-SUARTFSG-RAAR                         
080300        IF WS-RETOTBV-RAAR > +99.9                                        
080400           MOVE +99.9              TO                                     
080500                                U049-RETOTBV-RAAR                         
080600        ELSE                                                              
080700           IF WS-RETOTBV-RAAR < -99.9                                     
080800              MOVE -99.9              TO                                  
080900                                U049-RETOTBV-RAAR                         
081000           ELSE                                                           
081100              MOVE WS-RETOTBV-RAAR    TO                                  
081200                                U049-RETOTBV-RAAR                         
081300           END-IF                                                         
081400        END-IF                                                            
081500     END-IF                                                               
081600***  AF2/AF1 TG       ***                                                 
081700     SKIP2                                                                
081800     COMPUTE WS-RETOTBV-RAAR-TG = WS-RETOTBV-RAAR -                       
081900                                  WS-RETOTBV-FRAAR                        
082000     IF WS-RETOTBV-RAAR-TG > +99.9                                        
082100        MOVE +99.9              TO                                        
082200                                U049-RETOTBV-RAAR-TG                      
082300     ELSE                                                                 
082400        IF WS-RETOTBV-RAAR-TG < -99.9                                     
082500           MOVE -99.9              TO                                     
082600                                U049-RETOTBV-RAAR-TG                      
082700        ELSE                                                              
082800           MOVE WS-RETOTBV-RAAR-TG TO                                     
082900                                U049-RETOTBV-RAAR-TG                      
083000        END-IF                                                            
083100     END-IF                                                               
083200*************************     AVVIKELSE        ***                        
083300***  AVV STYCK AF4/AF3                                                    
083400     SKIP2                                                                
083500     IF U049-SULEVANT-FAAR = ZERO                                         
083600        CONTINUE                                                          
083700     ELSE                                                                 
083800        COMPUTE WS-RELEVANT-AAR    = (100 *                               
083900        (U049-SULEVANT-AAR - U049-SULEVANT-FAAR)) /                       
084000                               U049-SULEVANT-FAAR                         
084100        IF WS-RELEVANT-AAR > +99.9                                        
084200           MOVE +99.9              TO                                     
084300                                U049-RELEVANT-AAR                         
084400        ELSE                                                              
084500           IF WS-RELEVANT-AAR < -99.9                                     
084600              MOVE -99.9              TO                                  
084700                                U049-RELEVANT-AAR                         
084800           ELSE                                                           
084900              MOVE WS-RELEVANT-AAR    TO                                  
085000                                U049-RELEVANT-AAR                         
085100           END-IF                                                         
085200        END-IF                                                            
085300     END-IF                                                               
085400***  AVV SEK AF4/AF3                                                      
085500     SKIP2                                                                
085600     IF U049-SUARTFSG-FAAR = ZERO                                         
085700        CONTINUE                                                          
085800     ELSE                                                                 
085900        COMPUTE WS-REFSG-AAR       = (100 *                               
086000        (U049-SUARTFSG-AAR - U049-SUARTFSG-FAAR)) /                       
086100                               U049-SUARTFSG-FAAR                         
086200        IF WS-REFSG-AAR > +99.9                                           
086300           MOVE +99.9              TO                                     
086400                                U049-REFSG-AAR                            
086500        ELSE                                                              
086600           IF WS-REFSG-AAR < -99.9                                        
086700              MOVE -99.9              TO                                  
086800                                U049-REFSG-AAR                            
086900           ELSE                                                           
087000              MOVE WS-REFSG-AAR       TO                                  
087100                                U049-REFSG-AAR                            
087200           END-IF                                                         
087300        END-IF                                                            
087400     END-IF                                                               
087500***  AVV STYCK AF2/AF1                                                    
087600     IF U049-SULEVANT-FRAAR = ZERO                                        
087700        CONTINUE                                                          
087800     ELSE                                                                 
087900        COMPUTE WS-RELEVANT-RAAR   = (100 *                               
088000        (U049-SULEVANT-RAAR - U049-SULEVANT-FRAAR)) /                     
088100                               U049-SULEVANT-FRAAR                        
088200        IF WS-RELEVANT-RAAR > +99.9                                       
088300           MOVE +99.9              TO                                     
088400                                U049-RELEVANT-RAAR                        
088500        ELSE                                                              
088600           IF WS-RELEVANT-RAAR < -99.9                                    
088700              MOVE -99.9              TO                                  
088800                                U049-RELEVANT-RAAR                        
088900           ELSE                                                           
089000              MOVE WS-RELEVANT-RAAR   TO                                  
089100                                U049-RELEVANT-RAAR                        
089200           END-IF                                                         
089300        END-IF                                                            
089400     END-IF                                                               
089500     PERFORM S04-SKRIV-49-FIL                                             
089600     PERFORM S08-INITIERA-U049                                            
089700     .                                                                    
089800     EJECT                                                                
089900 S11-RELEASE-URVALSPOST    SECTION.                                       
090000     SKIP2                                                                
090100     MOVE I31-IDUSER                  TO WS-SORT-IDUSER                   
090200     MOVE I31-DAREGDAT                TO WS-SORT-DAREGDAT                 
090300     MOVE I31-TIREGTID                TO WS-SORT-TIREGTID                 
090400     IF I31-IDTRANS = '3202'                                              
090500        MOVE I31-AREA                 TO SORT231-AREA                     
090600     ELSE                                                                 
090700        MOVE I31-AREA                 TO SORT331-AREA                     
090800     END-IF                                                               
090900     MOVE ZERO                        TO WS-SORT-PRODSL                   
091000                                         WS-SORT-ARTNR                    
091100                                         WS-SORT-BEGREPP                  
091200     RELEASE SORTERAD-POST FROM WS-SORTERAD-AREA                          
091300     .                                                                    
091400     EJECT                                                                
091500 S12-SKRIV-51-FIL-URVAL    SECTION.                                       
091600     SKIP2                                                                
091700     IF WS-SORT-IDTRANS = '3202'                                          
091800        WRITE U251-POST FROM SORT231-AREA                                 
091900     ELSE                                                                 
092000        WRITE U351-POST FROM SORT331-AREA                                 
092100     END-IF                                                               
092200     MOVE '    '                 TO POSTSUM-TRANSTYP                      
092300     MOVE 'W33051'               TO POSTSUM-FDNAMN                        
092400     MOVE 'W33048D4'             TO POSTSUM-DDNAMN2                       
092500     CALL POSTSUM USING POSTSUM-PARM                                      
092600     .                                                                    
092700     EJECT                                                                
092800 Z-FINIT SECTION.                                                         
092900     SKIP2                                                                
093000     CLOSE W33031S                                                        
093100           W33047S                                                        
093200           W33049                                                         
093300           W33051                                                         
093400     MOVE 'S' TO POSTSUM-OPKOD                                            
093500     CALL POSTSUM USING POSTSUM-PARM                                      
093600     .                                                                    
093700     EJECT                                                                
