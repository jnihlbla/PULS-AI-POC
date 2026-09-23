000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9600400.                                                
000400 AUTHOR.         KARIN OLSSON.                                            
000500 DATE-WRITTEN.   93/02/17.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET UPPDATERAR EN LOGFIL MED EN NY LOG =                  
001100*        ÄNDRINGSLISTA.                                                   
001200*                                                                         
001300*        LOGGARNA SKA SPARAS I MINST 10 ÅR..                              
001400*        DESSUTOM SÅ SKA DE TRE SENASTE LOGGARNA SPARAS.                  
001500*        LOGGARNA KAN DÄRFÖR SPARAS MAX I 30 ÅR!                          
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 CONFIGURATION SECTION.                                                   
002600     SKIP2                                                                
002700 SPECIAL-NAMES.                                                           
002800     SKIP2                                                                
002900     CLASS NUMBERS IS '0' THROUGH '9' SPACE.                              
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200     SKIP2                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600     SKIP2                                                                
003601*    -COPY WY2000W4                                                       
003602     SKIP3                                                                
003700 77  IDPGM                       PIC X(8)    VALUE 'W9600400'.            
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000     SKIP2                                                                
004110 77  LMINIT                      PIC X(8)    VALUE 'LMINIT  '.            
004200 77  LMOPEN                      PIC X(8)    VALUE 'LMOPEN  '.            
004300 77  LMCLOSE                     PIC X(8)    VALUE 'LMCLOSE '.            
004400 77  LMFREE                      PIC X(8)    VALUE 'LMFREE  '.            
004500 77  LMMFIND                     PIC X(8)    VALUE 'LMMFIND '.            
004600 77  LMMREP                      PIC X(8)    VALUE 'LMMREP  '.            
004700 77  LMGET                       PIC X(8)    VALUE 'LMGET   '.            
004800 77  LMPUT                       PIC X(8)    VALUE 'LMPUT   '.            
004810 77  ISP-SELECT                  PIC X(8)    VALUE 'SELECT  '.            
004900 77  ISP-YES                     PIC X(8)    VALUE 'YES     '.            
005000 77  ISP-EXCLU                   PIC X(8)    VALUE 'EXCLU   '.            
005010 77  ISP-SHRW                    PIC X(8)    VALUE 'SHRW    '.            
005100 77  ISP-INPUT                   PIC X(8)    VALUE 'INPUT'.               
005200 77  ISP-OUTPUT                  PIC X(8)    VALUE 'OUTPUT'.              
005300 77  ISP-INVAR                   PIC X(8)    VALUE 'INVAR'.               
005400 77  ISP-VGET                    PIC X(8)    VALUE 'VGET    '.            
005500 77  ISP-VPUT                    PIC X(8)    VALUE 'VPUT    '.            
005600 77  ISP-VDEFINE                 PIC X(8)    VALUE 'VDEFINE '.            
005610 77  ISP-VRESET                  PIC X(8)    VALUE 'VRESET  '.            
005700 77  ISP-SHARED                  PIC X(8)    VALUE 'SHARED  '.            
005800 77  ISP-PROFILE                 PIC X(8)    VALUE 'PROFILE '.            
005900 77  CHAR                        PIC X(8)    VALUE 'CHAR    '.            
006000 77  FIXED                       PIC X(8)    VALUE 'FIXED   '.            
006100 77  VDEFINE-OPT                 PIC X(16)                                
006200                              VALUE '(COPY NOBSCAN)'.                     
006300 77  LOGFIL-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-LOGFIL                       VALUE 'J'.                   
006500     SKIP2                                                                
006600 77  DIFFLIST-EOF-SW             PIC X       VALUE 'N'.                   
006700     88  END-OF-DIFFLIST                     VALUE 'J'.                   
006800     SKIP2                                                                
006900 01  LOG-MEM-FINNS               PIC X       VALUE 'J'.                   
007000     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200*                                                                         
007300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007400     03  ISPLINK                 PIC X(8)    VALUE 'ISPLINK'.             
007500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007600     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007700     SKIP2                                                                
007800* --- PARAMETRAR TILL ABEND                                               
007900                                                                          
008000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008200     EJECT                                                                
008300 01  NOPARM                      PIC X       VALUE SPACE.                 
008400     SKIP2                                                                
008508 01  L-ALLOC-CMD               PIC S9(9) COMP  VALUE +149.                
008509 01  ALLOC-CMD.                                                           
008510     03  FILLER                PIC X(24)                                  
008511          VALUE 'CMD(ALLOC DD(FILDD) DSN('.                               
008514     03  ALLOC-DSN             PIC X(48).                                 
008515     03  FILLER                PIC X(43)                                  
008516          VALUE ') NEW MGMTCLAS(BACKUPC) SPACE(50,90) TRACKS'.            
008517     03  FILLER                PIC X(15)                                  
008518          VALUE ' CATALOG LRECL('.                                        
008519     03  ALLOC-LRECL           PIC 9(3).                                  
008520     03  FILLER                PIC X(16)                                  
008521          VALUE ') RECFM(F,B,A) )'.                                       
008522     SKIP2                                                                
008523 01  L-FREE-CMD                PIC S9(9) COMP  VALUE +20.                 
008524 01  FREE-CMD.                                                            
008525     03  FILLER                PIC X(20)                                  
008526          VALUE 'CMD(FREE DD(FILDD) )'.                                   
008550 01  ZERRSM                  PIC X(24).                                   
008600 01  N-ZERRSM                PIC X(8)    VALUE 'ZERRSM'.                  
008700 01  L-ZERRSM                PIC S9(9) COMP VALUE +24.                    
008710 01  ZERRLM                  PIC X(512).                                  
008720 01  N-ZERRLM                PIC X(8)    VALUE 'ZERRLM'.                  
008730 01  L-ZERRLM                PIC S9(9) COMP VALUE +512.                   
008740 01  ZLCDATE                 PIC X(8).                                    
008750 01  N-ZLCDATE               PIC X(8)    VALUE 'ZLCDATE'.                 
008760 01  L-ZLCDATE               PIC S9(9) COMP VALUE +8.                     
008800 01  ZLMDATE                 PIC X(8).                                    
008900 01  N-ZLMDATE               PIC X(8)    VALUE 'ZLMDATE'.                 
009000 01  L-ZLMDATE               PIC S9(9) COMP VALUE +8.                     
009100 01  ZLMTIME                 PIC X(5).                                    
009200 01  N-ZLMTIME               PIC X(8)    VALUE 'ZLMTIME'.                 
009300 01  L-ZLMTIME               PIC S9(9) COMP VALUE +5.                     
009400 01  ZLCNORC                 PIC S9(8) COMP.                              
009500 01  N-ZLCNORC               PIC X(8)    VALUE 'ZLCNORC'.                 
009600 01  L-ZLCNORC               PIC S9(9) COMP VALUE +4.                     
009700 01  ZLINORC                 PIC S9(8) COMP.                              
009800 01  N-ZLINORC               PIC X(8)    VALUE 'ZLINORC'.                 
009900 01  L-ZLINORC               PIC S9(9) COMP VALUE +4.                     
009910 01  ZLMNORC                 PIC S9(8) COMP.                              
009920 01  N-ZLMNORC               PIC X(8)    VALUE 'ZLMNORC'.                 
009930 01  L-ZLMNORC               PIC S9(9) COMP VALUE +4.                     
010000 01  ZLUSER                  PIC X(8).                                    
010100 01  N-ZLUSER                PIC X(8)    VALUE 'ZLUSER'.                  
010200 01  L-ZLUSER                PIC S9(9) COMP VALUE +8.                     
010300 01  LOGPDS                      PIC X(40).                               
010400 01  N-LOGPDS                    PIC X(8)    VALUE 'LOGPDS'.              
010500 01  L-LOGPDS                    PIC S9(9)   COMP VALUE +40.              
010600     SKIP2                                                                
010700 01  LOGMEM                      PIC X(8).                                
010800 01  N-LOGMEM                    PIC X(8)    VALUE 'LOGMEM'.              
010900 01  L-LOGMEM                    PIC S9(9)   COMP VALUE +8.               
011000     SKIP2                                                                
011100 01  LOGID                       PIC X(8).                                
011200 01  N-LOGID                     PIC X(8)    VALUE 'LOGID'.               
011300 01  L-LOGID                     PIC S9(9)   COMP VALUE +8.               
011400     SKIP2                                                                
011500 01  LOGDATE.                                                             
011600     03 LOGDATE-AA               PIC X(2).                                
011700     03 FILLER                   PIC X.                                   
011800     03 LOGDATE-MM               PIC X(2).                                
011900     03 FILLER                   PIC X.                                   
012000     03 LOGDATE-DD               PIC X(2).                                
012100 01  N-LOGDATE                   PIC X(8)    VALUE 'LOGDATE'.             
012200 01  L-LOGDATE                   PIC S9(9)   COMP VALUE +8.               
012300     SKIP2                                                                
012400 01  LOGTIME                     PIC X(6).                                
012500 01  N-LOGTIME                   PIC X(8)    VALUE 'LOGTIME'.             
012600 01  L-LOGTIME                   PIC S9(9)   COMP VALUE +6.               
012700     SKIP2                                                                
012800 01  DIFFLIST                    PIC X(48).                               
012900 01  N-DIFFLIST                  PIC X(8)    VALUE 'DIFFLIST'.            
013000 01  L-DIFFLIST                  PIC S9(9)   COMP VALUE +48.              
013100     SKIP2                                                                
013200 01  DDVAR1                      PIC X(8).                                
013300 01  N-DDVAR1                    PIC X(8)    VALUE 'DDVAR1'.              
013400 01  L-DDVAR1                    PIC S9(9)   COMP VALUE +8.               
013500     SKIP2                                                                
013600 01  DDVAR2                      PIC X(8).                                
013700 01  N-DDVAR2                    PIC X(8)    VALUE 'DDVAR2'.              
013800 01  L-DDVAR2                    PIC S9(9)   COMP VALUE +8.               
014700     SKIP2                                                                
014800 01  MEMPOST                     PIC X(150).                              
014900 01  FILLER REDEFINES MEMPOST.                                            
015000     03  MEMPOST-SKIP            PIC X.                                   
015100     03  MEMPOST-ID              PIC X(7).                                
015200     03  MEMPOST-CHANGED         PIC X(8).                                
015300     03  MEMPOST-AADDD           PIC 9(5).                                
015400     03  MEMPOST-BY              PIC X(4).                                
015500     03  MEMPOST-USERID          PIC X(7).                                
015600     03  FILLER                  PIC X(118).                              
015700 01  N-MEMPOST                   PIC X(8)    VALUE 'MEMPOST'.             
015800 01  L-MEMPOST                   PIC S9(9)   COMP VALUE +133.             
015900     SKIP2                                                                
016000 01  WDATALEN                    PIC X(8).                                
016100 01  N-WDATALEN                  PIC X(8)    VALUE 'WDATALEN'.            
016200 01  L-WDATALEN                  PIC S9(9)   COMP VALUE +8.               
016300     SKIP2                                                                
016400 01  PDSMAXLEN                   PIC S9(9)   COMP.                        
016500 01  LISTMAXLEN                  PIC S9(9)   COMP.                        
016600 01  WLOG-DGR-X                  PIC X(4).                                
016700 01  WLOG-ANTAL-X                PIC X(2).                                
016800 01  WLOG-AA                     PIC 9(4) COMP-3 VALUE 10.                
016900 01  WLOG-ANTAL                  PIC 9(2) COMP-3 VALUE 3.                 
017000 01  WSTRING                     PIC X(80).                               
017100 01  WDSNAME                     PIC X(48).                               
017110 01  WLOGPDS                     PIC X(40).                               
017200     EJECT                                                                
017300 01  CHANGED-AAMMDD.                                                      
017400     03  FILLER                  PIC X(6).                                
017500 01  CHANGED-AADDD               PIC 9(5).                                
017600 01  CURRLOG-AADDD               PIC 9(5).                                
017700 01  ANTAL-LOGGAR                PIC 9(2).                                
017800     SKIP2                                                                
017900 01  AKTUELL-TID.                                                         
018000     03  TID-TIMME               PIC X(2)    VALUE SPACE.                 
018100     03  TID-MINUT               PIC X(2)    VALUE SPACE.                 
018200     03  TID-SEKUND              PIC X(2)    VALUE SPACE.                 
018300     03  TID-100DELS-SEKUND      PIC X(2)    VALUE SPACE.                 
018400     SKIP2                                                                
018500 01  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
018600     SKIP2                                                                
018700 01  FILLER REDEFINES DAGENS-DATUM.                                       
018800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
018900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
019000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
019100     SKIP2                                                                
019200 01  DAGENS-AADDD.                                                        
019300     03  DAGENS-AA               PIC 9(2)   VALUE ZERO.                   
019400     03  DAGENS-DDD              PIC 9(3)   VALUE ZERO.                   
019500 01  LOG-MIN-AADDD-GRP.                                                   
019600     03  LOG-MIN-AA              PIC 9(2)   VALUE ZERO.                   
019700     03  LOG-MIN-DDD             PIC 9(3)   VALUE ZERO.                   
019710 01  LOG-MIN-AADDD REDEFINES LOG-MIN-AADDD-GRP                            
019720                                 PIC 9(5).                                
019800     EJECT                                                                
019900 01  TAB-IX                      PIC S9(5)  COMP-3 VALUE ZERO.            
020000 01  MAX-IX                      PIC S9(5)  COMP-3.                       
020100     SKIP2                                                                
020200 01  LOG-TAB.                                                             
020300     03  LOG-RAD  OCCURS 50000 PIC X(150).                                
020400     EJECT                                                                
020500*01  -COPY WDATAREA                                                       
020600     EJECT                                                                
020700*01  -COPY WDECAREA                                                       
020800     EJECT                                                                
020900 PROCEDURE DIVISION.                                                      
021000     SKIP2                                                                
021100     PERFORM A-INIT                                                       
021200                                                                          
021300     IF LOG-MEM-FINNS = JA                                                
021400       PERFORM B-SPARA-GAMLA-LOGGAR                                       
021500     END-IF                                                               
021600                                                                          
021700     PERFORM C-KOPIERA-DIFFLIST                                           
021800                                                                          
021900     IF LOG-MEM-FINNS = JA AND TAB-IX > 0                                 
022000       PERFORM D-SKRIV-GAMLA-LOGGAR                                       
022010       DISPLAY '       Existing log updated for ' LOGMEM                  
022020     ELSE                                                                 
022030       DISPLAY '       New log written for ' LOGMEM                       
022100     END-IF                                                               
022200                                                                          
022300     MOVE ZERO TO RETURN-CODE                                             
022400     PERFORM Z-FINIT                                                      
022500     GOBACK                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INIT SECTION.                                                          
022900     SKIP2                                                                
023000     ACCEPT DAGENS-DATUM  FROM DATE                                       
023100     ACCEPT AKTUELL-TID   FROM TIME                                       
023200                                                                          
023300     CALL ISPLINK USING ISP-VDEFINE N-ZERRSM ZERRSM CHAR                  
023400                         L-ZERRSM VDEFINE-OPT                             
023500     IF RETURN-CODE > 8                                                   
023600       DISPLAY 'KAN INTE VDEFFA ZERRSM'                                   
023700       PERFORM S99-ABEND                                                  
023800     END-IF                                                               
023900                                                                          
023910     CALL ISPLINK USING ISP-VDEFINE N-ZERRLM ZERRLM CHAR                  
023920                         L-ZERRLM VDEFINE-OPT                             
023930     IF RETURN-CODE > 8                                                   
023940       DISPLAY 'KAN INTE VDEFFA ZERRLM'                                   
023950       PERFORM S99-ABEND                                                  
023960     END-IF                                                               
023970                                                                          
023980     CALL ISPLINK USING ISP-VDEFINE N-ZLCDATE ZLCDATE CHAR                
023990                         L-ZLCDATE VDEFINE-OPT                            
023991     IF RETURN-CODE > 8                                                   
023992       DISPLAY 'KAN INTE VDEFFA ZLCDATE'                                  
023993       PERFORM S99-ABEND                                                  
023994     END-IF                                                               
023995                                                                          
024000     CALL ISPLINK USING ISP-VDEFINE N-ZLMDATE ZLMDATE CHAR                
024100                         L-ZLMDATE VDEFINE-OPT                            
024200     IF RETURN-CODE > 8                                                   
024300       DISPLAY 'KAN INTE VDEFFA ZLMDATE'                                  
024400       PERFORM S99-ABEND                                                  
024500     END-IF                                                               
024600                                                                          
024700     CALL ISPLINK USING ISP-VDEFINE N-ZLMTIME ZLMTIME CHAR                
024800                         L-ZLMTIME VDEFINE-OPT                            
024900     IF RETURN-CODE > 8                                                   
025000       DISPLAY 'KAN INTE VDEFFA ZLMTIME'                                  
025100       PERFORM S99-ABEND                                                  
025200     END-IF                                                               
025300                                                                          
025400     CALL ISPLINK USING ISP-VDEFINE N-ZLCNORC ZLCNORC FIXED               
025500                         L-ZLCNORC VDEFINE-OPT                            
025600     IF RETURN-CODE > 8                                                   
025700       DISPLAY 'KAN INTE VDEFFA ZLCNORC'                                  
025800       PERFORM S99-ABEND                                                  
025900     END-IF                                                               
026000                                                                          
026100     CALL ISPLINK USING ISP-VDEFINE N-ZLINORC ZLINORC FIXED               
026200                         L-ZLINORC VDEFINE-OPT                            
026300     IF RETURN-CODE > 8                                                   
026400       DISPLAY 'KAN INTE VDEFFA ZLINORC'                                  
026500       PERFORM S99-ABEND                                                  
026600     END-IF                                                               
026700                                                                          
026710     CALL ISPLINK USING ISP-VDEFINE N-ZLMNORC ZLMNORC FIXED               
026720                         L-ZLMNORC VDEFINE-OPT                            
026730     IF RETURN-CODE > 8                                                   
026740       DISPLAY 'KAN INTE VDEFFA ZLMNORC'                                  
026750       PERFORM S99-ABEND                                                  
026760     END-IF                                                               
026770                                                                          
026800     CALL ISPLINK USING ISP-VDEFINE N-ZLUSER ZLUSER CHAR                  
026900                         L-ZLUSER VDEFINE-OPT                             
027000     IF RETURN-CODE > 8                                                   
027100       DISPLAY 'KAN INTE VDEFFA ZLUSER'                                   
027200       PERFORM S99-ABEND                                                  
027300     END-IF                                                               
027400                                                                          
027500     CALL ISPLINK USING ISP-VDEFINE N-LOGPDS LOGPDS CHAR                  
027600                         L-LOGPDS VDEFINE-OPT                             
027700     IF RETURN-CODE > 0                                                   
027800       DISPLAY 'SAKNAR LOGPDS'                                            
027900       PERFORM S99-ABEND                                                  
028000     END-IF                                                               
028100                                                                          
028200     CALL ISPLINK USING ISP-VDEFINE N-LOGMEM LOGMEM CHAR                  
028300                         L-LOGMEM VDEFINE-OPT                             
028400     IF RETURN-CODE > 0                                                   
028500       DISPLAY 'SAKNAR LOGMEM'                                            
028600       PERFORM S99-ABEND                                                  
028700     END-IF                                                               
028800                                                                          
028900     CALL ISPLINK USING ISP-VDEFINE N-LOGID LOGID CHAR                    
029000                         L-LOGID VDEFINE-OPT                              
029100     IF RETURN-CODE > 0                                                   
029200       DISPLAY 'SAKNAR LOGID'                                             
029300       PERFORM S99-ABEND                                                  
029400     END-IF                                                               
029500                                                                          
029600     CALL ISPLINK USING ISP-VDEFINE N-LOGDATE LOGDATE CHAR                
029700                         L-LOGDATE VDEFINE-OPT                            
029800     IF RETURN-CODE > 0                                                   
029900       DISPLAY 'SAKNAR LOGDATE'                                           
030000       PERFORM S99-ABEND                                                  
030100     END-IF                                                               
030200                                                                          
030300     CALL ISPLINK USING ISP-VDEFINE N-LOGTIME LOGTIME CHAR                
030400                         L-LOGTIME VDEFINE-OPT                            
030500     IF RETURN-CODE > 0                                                   
030600       DISPLAY 'SAKNAR LOGTIME'                                           
030700       PERFORM S99-ABEND                                                  
030800     END-IF                                                               
030900                                                                          
031000     CALL ISPLINK USING ISP-VDEFINE N-DIFFLIST DIFFLIST CHAR              
031100                         L-DIFFLIST VDEFINE-OPT                           
031200     IF RETURN-CODE > 0                                                   
031300       DISPLAY 'SAKNAR DIFFLIST'                                          
031400       PERFORM S99-ABEND                                                  
031500     END-IF                                                               
031700                                                                          
031800     CALL ISPLINK USING ISP-VDEFINE N-DDVAR1 DDVAR1 CHAR                  
031900                         L-DDVAR1 VDEFINE-OPT                             
032000     IF RETURN-CODE > 8                                                   
032100       DISPLAY 'KAN INTE VDEFFA DDVAR1'                                   
032200       PERFORM S99-ABEND                                                  
032300     END-IF                                                               
032400                                                                          
032500     CALL ISPLINK USING ISP-VDEFINE N-DDVAR2 DDVAR2 CHAR                  
032600                         L-DDVAR2 VDEFINE-OPT                             
032700     IF RETURN-CODE > 8                                                   
032800       DISPLAY 'KAN INTE VDEFFA DDVAR2'                                   
032900       PERFORM S99-ABEND                                                  
033000     END-IF                                                               
033100                                                                          
034600     CALL ISPLINK USING ISP-VDEFINE N-MEMPOST MEMPOST CHAR                
034700                         L-MEMPOST VDEFINE-OPT                            
034800     IF RETURN-CODE > 8                                                   
034900       DISPLAY 'KAN INTE VDEFFA MEMPOST'                                  
035000       PERFORM S99-ABEND                                                  
035100     END-IF                                                               
035200                                                                          
035300     CALL ISPLINK USING ISP-VDEFINE N-WDATALEN WDATALEN CHAR              
035400                         L-WDATALEN VDEFINE-OPT                           
035500     IF RETURN-CODE > 8                                                   
035600       DISPLAY 'KAN INTE VDEFFA WDATALEN'                                 
035700       PERFORM S99-ABEND                                                  
035800     END-IF                                                               
035900                                                                          
036000* DDVAR1 KOPPLAS FÖRST TILL DEN GAMLA LOG-MEDLEMMEN                       
036100                                                                          
036110     MOVE SPACE TO WLOGPDS                                                
036120     STRING '''' LOGPDS ''''                                              
036130           DELIMITED BY SPACE                                             
036140           INTO WLOGPDS                                                   
036150                                                                          
036200     CALL ISPLINK USING LMINIT N-DDVAR1 NOPARM NOPARM NOPARM              
036300          NOPARM NOPARM NOPARM WLOGPDS                                    
036400     IF RETURN-CODE NOT = 0                                               
036500       DISPLAY 'HITTAR INTE LOGPDS ' RETURN-CODE                          
036600       PERFORM S99-ABEND                                                  
036700     END-IF                                                               
036800                                                                          
036900     CALL ISPLINK USING LMOPEN DDVAR1 ISP-INPUT                           
037000                  N-WDATALEN                                              
037100     IF RETURN-CODE NOT = 0                                               
037200       DISPLAY 'KAN INTE ÖPPNA LOGPDS ' RETURN-CODE                       
037300       CALL ISPLINK USING LMFREE DDVAR1                                   
037400       PERFORM S99-ABEND                                                  
037500     END-IF                                                               
037600                                                                          
038500     MOVE WDATALEN TO DEC-IDFRIDATA                                       
038600     MOVE 8        TO DEC-KVHELTAL                                        
038700     MOVE 0        TO DEC-KVDECIMAL                                       
038800                                                                          
038900     CALL WDECEDIT USING DEC-WDECAREA                                     
039000                                                                          
039100     IF DEC-KDSVAR-FEL                                                    
039200       DISPLAY 'FEL FRÅN WDECEDIT'                                        
039300       PERFORM S99-ABEND                                                  
039400     END-IF                                                               
039500                                                                          
039600     MOVE DEC-IDEDITDATA TO PDSMAXLEN ALLOC-LRECL                         
039700                                                                          
039800     MOVE JA TO LOG-MEM-FINNS                                             
039900     CALL ISPLINK USING LMMFIND DDVAR1 LOGMEM NOPARM                      
040000                  NOPARM NOPARM NOPARM ISP-YES                            
040100     IF RETURN-CODE > 8                                                   
040200       DISPLAY 'KAN INTE GÖRA FIND I LOGPDS ' RETURN-CODE                 
040300       PERFORM S99-ABEND                                                  
040400     END-IF                                                               
040500                                                                          
040600     IF RETURN-CODE > 0                                                   
040700       MOVE NEJ TO LOG-MEM-FINNS                                          
040800       MOVE ZERO TO ZLINORC                                               
041500     END-IF                                                               
041501                                                                          
041510     IF ZLCDATE = SPACE                                                   
041520       MOVE LOGDATE TO ZLCDATE ZLMDATE                                    
041530     ELSE                                                                 
041540       MOVE LOGDATE TO ZLMDATE                                            
041550     END-IF                                                               
041600                                                                          
041700     MOVE LOGTIME TO ZLMTIME                                              
041800                                                                          
041900     MOVE LOGID  TO ZLUSER                                                
042000     MOVE ZERO TO ZLCNORC                                                 
042100                                                                          
042200     CALL ISPLINK USING LMCLOSE DDVAR1                                    
042300     CALL ISPLINK USING LMFREE DDVAR1                                     
042400                                                                          
042500* DDVAR1 KOPPLAS TILL LÄSNING AV GAMLA LOGFILEN                           
042600                                                                          
042700     MOVE SPACE TO WDSNAME                                                
042800     STRING '''' LOGPDS '.' LOGMEM ''''                                   
042900           DELIMITED BY SPACE                                             
043000           INTO WDSNAME                                                   
043100                                                                          
043110     MOVE WDSNAME TO ALLOC-DSN                                            
043120                                                                          
043200     CALL ISPLINK USING LMINIT N-DDVAR1 NOPARM NOPARM NOPARM              
043300          NOPARM NOPARM NOPARM WDSNAME                                    
043400     IF RETURN-CODE = 0                                                   
043500       MOVE JA TO LOG-MEM-FINNS                                           
043510       MOVE NEJ TO LOGFIL-EOF-SW                                          
043600       CALL ISPLINK USING LMOPEN DDVAR1 ISP-INPUT                         
043700                    N-WDATALEN                                            
043800       IF RETURN-CODE NOT = 0                                             
043900         DISPLAY 'KAN INTE ÖPPNA GAMLA LOGFILEN ' RETURN-CODE             
044000         CALL ISPLINK USING LMFREE DDVAR1                                 
044100         PERFORM S99-ABEND                                                
044200       END-IF                                                             
044210     ELSE                                                                 
044220       CALL ISPLINK USING ISP-SELECT L-ALLOC-CMD  ALLOC-CMD               
044230       IF RETURN-CODE > 4                                                 
044240         DISPLAY 'KAN INTE ALLOKERA NY LOGFIL ' RETURN-CODE               
044260         PERFORM S99-ABEND                                                
044270       END-IF                                                             
044280       CALL ISPLINK USING ISP-SELECT L-FREE-CMD  FREE-CMD                 
044300     END-IF                                                               
044400                                                                          
044500* DDVAR2 KOPPLAS TILL LÄSNING AV DIFFLISTAN                               
044600                                                                          
044700     CALL ISPLINK USING LMINIT N-DDVAR2 NOPARM NOPARM NOPARM              
044800          NOPARM NOPARM NOPARM DIFFLIST                                   
044900     IF RETURN-CODE NOT = 0                                               
045000       DISPLAY 'HITTAR INTE DIFFLIST ' RETURN-CODE                        
045100       CALL ISPLINK USING LMCLOSE DDVAR1                                  
045200       CALL ISPLINK USING LMFREE DDVAR1                                   
045300       PERFORM S99-ABEND                                                  
045400     END-IF                                                               
045401     MOVE NEJ TO DIFFLIST-EOF-SW                                          
045500                                                                          
045600     CALL ISPLINK USING LMOPEN DDVAR2 ISP-INPUT                           
045700                  N-WDATALEN                                              
045800     IF RETURN-CODE NOT = 0                                               
045900       DISPLAY 'KAN INTE ÖPPNA DIFFLIST ' RETURN-CODE                     
046000       CALL ISPLINK USING LMCLOSE DDVAR1                                  
046100       CALL ISPLINK USING LMFREE DDVAR1                                   
046200       CALL ISPLINK USING LMFREE DDVAR2                                   
046300       PERFORM S99-ABEND                                                  
046400     END-IF                                                               
046500                                                                          
046600     MOVE WDATALEN TO DEC-IDFRIDATA                                       
046700     MOVE 8        TO DEC-KVHELTAL                                        
046800     MOVE 0        TO DEC-KVDECIMAL                                       
046900                                                                          
047000     CALL WDECEDIT USING DEC-WDECAREA                                     
047100                                                                          
047200     IF DEC-KDSVAR-FEL                                                    
047300       DISPLAY 'FEL FRÅN WDECEDIT'                                        
047400       PERFORM S99-ABEND                                                  
047500     END-IF                                                               
047600                                                                          
047700     MOVE DEC-IDEDITDATA TO LISTMAXLEN                                    
047800                                                                          
053400     MOVE 'IDAG' TO DAT-KDDATFORM                                         
053500                                                                          
053600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
053700                   DAT-O-TIDATUM DAT-KDSVAR                               
053800     IF DAT-KDSVAR-OK                                                     
053900       MOVE DAT-TIAADDD TO DAGENS-AADDD LOG-MIN-AADDD                     
054000       IF DAGENS-AA >= WLOG-AA                                            
054100         SUBTRACT WLOG-AA FROM LOG-MIN-AA                                 
054200       ELSE                                                               
054300         COMPUTE LOG-MIN-AA = DAGENS-AA + (100 - WLOG-AA)                 
054400       END-IF                                                             
054500     ELSE                                                                 
054600       DISPLAY 'EJ OK FRÅN DATKONV'                                       
054700       PERFORM S99-ABEND                                                  
054800     END-IF                                                               
054900                                                                          
055000     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
055100     STRING LOGDATE-AA LOGDATE-MM LOGDATE-DD                              
055200            DELIMITED BY SIZE                                             
055300            INTO CHANGED-AAMMDD                                           
055400     MOVE CHANGED-AAMMDD TO DAT-I-TIDATUM                                 
055500                                                                          
055600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
055700                   DAT-O-TIDATUM DAT-KDSVAR                               
055800     IF DAT-KDSVAR-OK                                                     
055900       MOVE DAT-TIAADDD TO CHANGED-AADDD                                  
056000     ELSE                                                                 
056100       DISPLAY 'EJ OK FRÅN DATKONV'                                       
056200       PERFORM S99-ABEND                                                  
056300     END-IF                                                               
056400     .                                                                    
056500     EJECT                                                                
056600 B-SPARA-GAMLA-LOGGAR     SECTION.                                        
056700     SKIP2                                                                
056900     MOVE ZERO TO ANTAL-LOGGAR CURRLOG-AADDD TAB-IX                       
057000     PERFORM S01-LAES-LOGFIL                                              
057100     IF NOT END-OF-LOGFIL                                                 
057200       IF MEMPOST-ID = 'NEWLOG'                                           
057300         MOVE MEMPOST-AADDD TO CURRLOG-AADDD                              
057400       ELSE                                                               
057500         DISPLAY 'SAKNAR DATUM PÅ FÖRSTA LOGGEN'                          
057600         PERFORM S99-ABEND                                                
057700       END-IF                                                             
057702       MOVE CURRLOG-AADDD   TO TMP1-YYDDD                                 
057703       MOVE LOG-MIN-AADDD   TO TMP2-YYDDD                                 
057710       PERFORM WY2000P4                                                   
057800       PERFORM UNTIL END-OF-LOGFIL                                        
057900         OR (ANTAL-LOGGAR > 1 AND TMP1-YYDDD < TMP2-YYDDD)                
057910                                                                          
058000         ADD 1 TO TAB-IX                                                  
058100         IF TAB-IX > 50000                                                
058200           DISPLAY 'TABELL FULL! ÖKA, KOMPILERA OM!'                      
058300           PERFORM S99-ABEND                                              
058400         END-IF                                                           
058500         MOVE MEMPOST TO LOG-RAD (TAB-IX)                                 
058600         PERFORM S01-LAES-LOGFIL                                          
058700                                                                          
058800         IF NOT END-OF-LOGFIL AND MEMPOST-ID = 'NEWLOG'                   
058900           MOVE MEMPOST-AADDD TO CURRLOG-AADDD                            
059010           MOVE CURRLOG-AADDD   TO TMP1-YYDDD                             
059020           MOVE LOG-MIN-AADDD   TO TMP2-YYDDD                             
059030           PERFORM WY2000P4                                               
059040           ADD 1 TO ANTAL-LOGGAR                                          
059100         END-IF                                                           
059101                                                                          
059200       END-PERFORM                                                        
059300     END-IF                                                               
059301                                                                          
059310     CALL ISPLINK USING LMCLOSE DDVAR1                                    
059320     CALL ISPLINK USING LMFREE DDVAR1                                     
059400     .                                                                    
059500     EJECT                                                                
059600 C-KOPIERA-DIFFLIST   SECTION.                                            
059700     SKIP2                                                                
059710* DDVAR1 KOPPLAS TILL SKRIVNING AV LOGFIL                                 
059720                                                                          
059730     CALL ISPLINK USING LMINIT N-DDVAR1 NOPARM NOPARM NOPARM              
059740          NOPARM NOPARM NOPARM WDSNAME  NOPARM NOPARM NOPARM              
059750          ISP-EXCLU                                                       
059760     IF RETURN-CODE NOT = 0                                               
059770     DISPLAY 'KAN INTE INITIERA ' WDSNAME                                 
059780             'FÖR SKRIVNING ' RETURN-CODE                                 
059790       DISPLAY ZERRSM                                                     
059791       DISPLAY ZERRLM                                                     
059794       CALL ISPLINK USING LMCLOSE DDVAR2                                  
059795       CALL ISPLINK USING LMFREE DDVAR2                                   
059796       PERFORM S99-ABEND                                                  
059797     END-IF                                                               
059798                                                                          
059799     CALL ISPLINK USING LMOPEN DDVAR1 ISP-OUTPUT                          
059800     IF RETURN-CODE NOT = 0                                               
059801       DISPLAY 'KAN INTE ÖPPNA LOGFIL FÖR SKRIVNING ' RETURN-CODE         
059804       CALL ISPLINK USING LMCLOSE DDVAR2                                  
059805       CALL ISPLINK USING LMFREE DDVAR2                                   
059806       CALL ISPLINK USING LMFREE DDVAR1                                   
059807       PERFORM S99-ABEND                                                  
059808     END-IF                                                               
059809                                                                          
059810     PERFORM S02-LAES-DIFFLIST                                            
059900     IF NOT END-OF-DIFFLIST                                               
060000       MOVE SPACE TO MEMPOST                                              
060100       MOVE '1'          TO MEMPOST-SKIP                                  
060200       MOVE 'NEWLOG'     TO MEMPOST-ID                                    
060300       MOVE 'CHANGED'    TO MEMPOST-CHANGED                               
060400       MOVE CHANGED-AADDD TO MEMPOST-AADDD                                
060500       MOVE ' BY '       TO MEMPOST-BY                                    
060600       MOVE LOGID        TO MEMPOST-USERID                                
060700       PERFORM UNTIL END-OF-DIFFLIST                                      
060800         PERFORM S11-SKRIV-LOGFIL                                         
060900         PERFORM S02-LAES-DIFFLIST                                        
061000       END-PERFORM                                                        
061100     END-IF                                                               
061101                                                                          
061110     CALL ISPLINK USING LMCLOSE DDVAR2                                    
061120     CALL ISPLINK USING LMFREE DDVAR2                                     
061200     .                                                                    
061300     EJECT                                                                
061400 D-SKRIV-GAMLA-LOGGAR  SECTION.                                           
061500     SKIP2                                                                
061600     MOVE TAB-IX TO MAX-IX                                                
061700     MOVE 1 TO TAB-IX                                                     
061800     PERFORM UNTIL TAB-IX > MAX-IX                                        
061900       MOVE LOG-RAD (TAB-IX) TO MEMPOST                                   
062000       PERFORM S11-SKRIV-LOGFIL                                           
062100       ADD 1 TO TAB-IX                                                    
062200     END-PERFORM                                                          
062300     .                                                                    
062400     EJECT                                                                
062500 Z-FINIT   SECTION.                                                       
062600     SKIP2                                                                
062610     CALL ISPLINK USING LMCLOSE DDVAR1                                    
062620     CALL ISPLINK USING LMFREE DDVAR1                                     
062630                                                                          
062700     IF ZLINORC = ZERO                                                    
062800       MOVE ZLCNORC TO ZLINORC                                            
062900     END-IF                                                               
062901     MOVE ZERO TO ZLMNORC                                                 
062902                                                                          
062903* DDVAR1 KOPPLAS TILL SKRIVNING AV LOGPDS                                 
062904                                                                          
062905     CALL ISPLINK USING LMINIT N-DDVAR1 NOPARM NOPARM NOPARM              
062906          NOPARM NOPARM NOPARM WLOGPDS  NOPARM NOPARM NOPARM              
062907          ISP-SHRW                                                        
062908     IF RETURN-CODE NOT = 0                                               
062909     DISPLAY 'KAN INTE INITIERA LOGPDS FÖR SKRIVNING ' RETURN-CODE        
062915       PERFORM S99-ABEND                                                  
062916     END-IF                                                               
062917                                                                          
062918     CALL ISPLINK USING LMOPEN DDVAR1 ISP-OUTPUT                          
062919     IF RETURN-CODE NOT = 0                                               
062920       DISPLAY 'KAN INTE ÖPPNA LOGPDS FÖR SKRIVNING ' RETURN-CODE         
062927       CALL ISPLINK USING LMFREE DDVAR1                                   
062928       PERFORM S99-ABEND                                                  
062929     END-IF                                                               
062930                                                                          
062931     MOVE SPACE TO MEMPOST                                                
062932     CALL ISPLINK USING LMPUT DDVAR1 ISP-INVAR N-MEMPOST                  
062933                           PDSMAXLEN                                      
062934     IF RETURN-CODE NOT = 0                                               
062940       DISPLAY 'KAN INTE SKRIVA LOG-MEDLEM ' RETURN-CODE                  
062941       CALL ISPLINK USING LMCLOSE DDVAR1                                  
062942       CALL ISPLINK USING LMFREE DDVAR1                                   
062950       PERFORM S99-ABEND                                                  
062960     END-IF                                                               
062970                                                                          
063000     CALL ISPLINK USING LMMREP DDVAR1 LOGMEM ISP-YES                      
063500     CALL ISPLINK USING LMCLOSE DDVAR1                                    
063600     CALL ISPLINK USING LMFREE DDVAR1                                     
063601                                                                          
063610     CALL ISPLINK USING ISP-VRESET                                        
063700     .                                                                    
063800     EJECT                                                                
063900 S01-LAES-LOGFIL     SECTION.                                             
064000     SKIP2                                                                
064100     CALL ISPLINK USING LMGET DDVAR1 ISP-INVAR N-MEMPOST                  
064200                  N-WDATALEN PDSMAXLEN                                    
064300     IF RETURN-CODE > 8                                                   
064400       DISPLAY 'KAN INTE LÄSA LOGFIL ' RETURN-CODE                        
064500       PERFORM S99-ABEND                                                  
064600     END-IF                                                               
064700     IF RETURN-CODE = 8                                                   
064800       SET END-OF-LOGFIL TO TRUE                                          
064900     END-IF                                                               
065000     .                                                                    
065100     EJECT                                                                
065200 S02-LAES-DIFFLIST   SECTION.                                             
065300     SKIP2                                                                
065400     CALL ISPLINK USING LMGET DDVAR2 ISP-INVAR N-MEMPOST                  
065500                  N-WDATALEN LISTMAXLEN                                   
065600     IF RETURN-CODE > 8                                                   
065700       DISPLAY 'KAN INTE LÄSA DIFFLIST ' RETURN-CODE                      
065800       PERFORM S99-ABEND                                                  
065900     END-IF                                                               
066000     IF RETURN-CODE = 8                                                   
066100       SET END-OF-DIFFLIST TO TRUE                                        
066200     END-IF                                                               
066300     .                                                                    
066400 S11-SKRIV-LOGFIL    SECTION.                                             
066500     SKIP2                                                                
066600     CALL ISPLINK USING LMPUT DDVAR1 ISP-INVAR N-MEMPOST                  
066700                           PDSMAXLEN                                      
066800     IF RETURN-CODE NOT = 0                                               
066900       DISPLAY 'KAN INTE SKRIVA LOGFIL ' RETURN-CODE                      
067000       PERFORM S99-ABEND                                                  
067100     END-IF                                                               
067200     ADD 1 TO ZLCNORC                                                     
067300     .                                                                    
067400     EJECT                                                                
067500 S99-ABEND SECTION.                                                       
067600     SKIP2                                                                
067700     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
067800     .                                                                    
067910     EJECT                                                                
068000*    -COPY WY2000P4                                                       
