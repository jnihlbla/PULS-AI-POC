000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9600600.                                                
000400 AUTHOR.         KARIN OLSSON.                                            
000500 DATE-WRITTEN.  93/03/18.                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ANROPAS VID BROWSE AV EN LOGLISTA.                               
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600     SKIP2                                                                
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(8)    VALUE 'W9600600'.            
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003000     SKIP2                                                                
003100 77  LOGMEM-EOF-SW               PIC X       VALUE 'N'.                   
003200     88  END-OF-LOGMEM                       VALUE 'J'.                   
003620 77  WPANEL                      PIC X(8)    VALUE 'LOGBR   '.            
003700     SKIP2                                                                
003800 77  ISP-TBCREATE                PIC X(8)    VALUE 'TBCREATE'.            
003801 77  ISP-TBADD                   PIC X(8)    VALUE 'TBADD   '.            
003802 77  ISP-TBMOD                   PIC X(8)    VALUE 'TBMOD   '.            
003803 77  ISP-TBTOP                   PIC X(8)    VALUE 'TBTOP   '.            
003804 77  ISP-TBBOTTOM                PIC X(8)    VALUE 'TBBOTTOM'.            
003805 77  ISP-TBSKIP                  PIC X(8)    VALUE 'TBSKIP  '.            
003806 77  ISP-TBDISPL                 PIC X(8)    VALUE 'TBDISPL '.            
003807 77  ISP-TBEND                   PIC X(8)    VALUE 'TBEND   '.            
003810 77  ISP-BRIF                    PIC X(8)    VALUE 'BRIF    '.            
003900 77  ISP-VGET                    PIC X(8)    VALUE 'VGET    '.            
004000 77  ISP-VPUT                    PIC X(8)    VALUE 'VPUT    '.            
004100 77  ISP-VDEFINE                 PIC X(8)    VALUE 'VDEFINE '.            
004200 77  ISP-VDELETE                 PIC X(8)    VALUE 'VDELETE '.            
004300 77  ISP-VRESET                  PIC X(8)    VALUE 'VRESET  '.            
004400 77  ISP-DISPLAY                 PIC X(8)    VALUE 'DISPLAY '.            
004500 77  ISP-SHARED                  PIC X(8)    VALUE 'SHARED  '.            
004600 77  ISP-PROFILE                 PIC X(8)    VALUE 'PROFILE '.            
004700 77  ISP-CONTROL                 PIC X(8)    VALUE 'CONTROL '.            
004800 77  ISP-CANCEL                  PIC X(8)    VALUE 'CANCEL  '.            
004900 77  ISP-RETURN                  PIC X(8)    VALUE 'RETURN  '.            
005000 77  ISP-ERRORS                  PIC X(8)    VALUE 'ERRORS  '.            
005100 77  SETMSG                      PIC X(8)    VALUE 'SETMSG  '.            
005200 77  CHAR                        PIC X(8)    VALUE 'CHAR    '.            
005300 77  PACK                        PIC X(8)    VALUE 'PACK    '.            
005400 77  VDEFINE-OPT                 PIC X(16)                                
005500                              VALUE '(COPY NOBSCAN)'.                     
005600 77  LMINIT                      PIC X(8)    VALUE 'LMINIT  '.            
005700 77  LMOPEN                      PIC X(8)    VALUE 'LMOPEN  '.            
005710 77  LMMFIND                     PIC X(8)    VALUE 'LMMFIND '.            
005800 77  LMGET                       PIC X(8)    VALUE 'LMGET   '.            
005900 77  LMFREE                      PIC X(8)    VALUE 'LMFREE  '.            
006000 77  LMCLOSE                     PIC X(8)    VALUE 'LMCLOSE '.            
006100 77  ISP-INPUT                   PIC X(8)    VALUE 'INPUT   '.            
006200 77  ISP-INVAR                   PIC X(8)    VALUE 'INVAR   '.            
006300 77  ISP-REPLACE                 PIC X(8)    VALUE 'REPLACE '.            
006400 77  ISP-SHR                     PIC X(8)    VALUE 'SHR     '.            
006410 77  ISP-NOWRITE                 PIC X(8)    VALUE 'NOWRITE '.            
006420 77  ISP-SHARE                   PIC X(8)    VALUE 'SHARE   '.            
006430 77  TABELL                      PIC X(8)    VALUE 'RADTAB1 '.            
006431 77  TABSKIP                     PIC S9(9)   VALUE -1  COMP.              
006432 77  TAB-KEYS                    PIC X(8)    VALUE '(LSTART)'.            
006440 77  TAB-NAMES                   PIC X(22)                                
006450                    VALUE '(S LDATE LUSERID LMAX)'.                       
006500     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007000     03  ISPLINK                 PIC X(8)    VALUE 'ISPLINK'.             
007100     03  W9600610                PIC X(8)    VALUE 'W9600610'.            
007200     03  W9600620                PIC X(8)    VALUE 'W9600620'.            
007400     03  WDELETE                 PIC X(8)    VALUE 'WDELETE'.             
007500     EJECT                                                                
007600* --- PARAMETRAR TILL ABEND                                               
007700                                                                          
007800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008000     EJECT                                                                
008100* --- PARAMETRAR TILL WDECEDIT                                            
008200                                                                          
008300*01  -COPY WDECAREA                                                       
008400     EJECT                                                                
008700 01  WDUMMY                      PIC X       VALUE SPACE.                 
008800 01  WDSNAME                     PIC X(46).                               
008900 01  RECFM                       PIC X(2)    VALUE SPACE.                 
009000 01  LRECL                       PIC S9(6)   COMP.                        
009100 01  MAXLRECL                    PIC S9(9)   COMP SYNC.                   
009200 01  READPTR                     PIC S9(8)   COMP SYNC.                   
009300 01  DATAPTR                     PIC S9(8)   COMP SYNC.                   
009500 01  TAB-IX                      PIC S9(5)   COMP VALUE ZERO.             
009600     SKIP2                                                                
009700 01  VARLRECL                    PIC X(8).                                
009800 01  N-VARLRECL                  PIC X(8)    VALUE 'VARLRECL'.            
009900 01  L-VARLRECL                  PIC S9(9)   COMP VALUE +8.               
010000     SKIP2                                                                
010100 01  ZERRMSG                     PIC X(8).                                
010200 01  N-ZERRMSG                   PIC X(8)    VALUE 'ZERRMSG'.             
010300 01  L-ZERRMSG                   PIC S9(9)   COMP VALUE +8.               
010400     SKIP2                                                                
010411 01  ZTDSELS                     PIC 9(4).                                
010420 01  N-ZTDSELS                   PIC X(8)    VALUE 'ZTDSELS'.             
010430 01  L-ZTDSELS                   PIC S9(9)   COMP VALUE +4.               
010440     SKIP2                                                                
010451 01  ZTDTOP                      PIC 9(6).                                
010460 01  N-ZTDTOP                    PIC X(8)    VALUE 'ZTDTOP'.              
010470 01  L-ZTDTOP                    PIC S9(9)   COMP VALUE +6.               
010480     SKIP2                                                                
010500 01  DDVAR                       PIC X(8).                                
010600 01  N-DDVAR                     PIC X(8)    VALUE 'DDVAR'.               
010700 01  L-DDVAR                     PIC S9(9)   COMP VALUE +8.               
010800     SKIP2                                                                
010900 01  LOGLIB                      PIC X(44).                               
011000 01  N-LOGLIB                    PIC X(8)    VALUE 'LOGLIB'.              
011100 01  L-LOGLIB                    PIC S9(9)   COMP VALUE +44.              
011200     SKIP2                                                                
011300 01  LOGMEM                      PIC X(8).                                
011400 01  N-LOGMEM                    PIC X(8)    VALUE 'LOGMEM'.              
011500 01  L-LOGMEM                    PIC S9(9)   COMP VALUE +8.               
011600     SKIP2                                                                
011700 01  LIBRECFM                    PIC X(4).                                
011800 01  N-LIBRECFM                  PIC X(8)    VALUE 'LIBRECFM'.            
011900 01  L-LIBRECFM                  PIC S9(9)   COMP VALUE +4.               
012000     SKIP2                                                                
012100 01  LIBLRECL                    PIC X(8).                                
012200 01  N-LIBLRECL                  PIC X(8)    VALUE 'LIBLRECL'.            
012300 01  L-LIBLRECL                  PIC S9(9)   COMP VALUE +8.               
012400     SKIP2                                                                
012500 01  RADLRECL                    PIC 9(3)    COMP-3.                      
012600 01  N-RADLRECL                  PIC X(8)    VALUE 'RADLRECL'.            
012700 01  L-RADLRECL                  PIC S9(9)   COMP VALUE +2.               
012800     SKIP2                                                                
012810 01  TOPRADNR                    PIC 9(5)    COMP-3.                      
012820 01  N-TOPRADNR                  PIC X(8)    VALUE 'TOPRADNR'.            
012830 01  L-TOPRADNR                  PIC S9(9)   COMP VALUE +3.               
012840     SKIP2                                                                
012850 01  MAXRADNR                    PIC 9(5)    COMP-3.                      
012860 01  N-MAXRADNR                  PIC X(8)    VALUE 'MAXRADNR'.            
012870 01  L-MAXRADNR                  PIC S9(9)   COMP VALUE +3.               
012880     SKIP2                                                                
012890 01  S                           PIC X(1).                                
012891 01  N-S                         PIC X(8)    VALUE 'S     '.              
012892 01  L-S                         PIC S9(9)   COMP VALUE +1.               
012893     SKIP2                                                                
012894 01  LDATE                       PIC X(5).                                
012895 01  N-LDATE                     PIC X(8)    VALUE 'LDATE  '.             
012896 01  L-LDATE                     PIC S9(9)   COMP VALUE +5.               
012897     SKIP2                                                                
012898 01  LUSERID                     PIC X(7).                                
012899 01  N-USERID                    PIC X(8)    VALUE 'LUSERID'.             
012900 01  L-USERID                    PIC S9(9)   COMP VALUE +7.               
012901     SKIP2                                                                
012902 01  OLD-LSTART                  PIC 9(5).                                
012903 01  LSTART                      PIC 9(5).                                
012904 01  N-LSTART                    PIC X(8)    VALUE 'LSTART '.             
012905 01  L-LSTART                    PIC S9(9)   COMP VALUE +5.               
012906     SKIP2                                                                
012907 01  LMAX                        PIC 9(5).                                
012908 01  N-LMAX                      PIC X(8)    VALUE 'LMAX '.               
012909 01  L-LMAX                      PIC S9(9)   COMP VALUE +5.               
012910     SKIP2                                                                
012911 01  MEMPOST                     PIC X(150).                              
012912 01  FILLER REDEFINES MEMPOST.                                            
012913     03  FILLER                  PIC X.                                   
012914     03  MEMPOST-ID              PIC X(7).                                
012915     03  FILLER                  PIC X(8).                                
012916     03  MEMPOST-AADDD           PIC 9(5).                                
012917     03  FILLER                  PIC X(4).                                
012918     03  MEMPOST-USERID          PIC X(7).                                
012919     03  FILLER                  PIC X(118).                              
013020 01  N-MEMPOST                   PIC X(8)    VALUE 'MEMPOST'.             
013100 01  L-MEMPOST                   PIC S9(9)   COMP VALUE +150.             
013200     SKIP2                                                                
020600 01  RADTAB.                                                              
020700     03  DATARAD OCCURS 20000 PIC X(150).                                 
020800     EJECT                                                                
020900 PROCEDURE DIVISION.                                                      
021000     SKIP2                                                                
021100     MOVE ZERO TO RETURN-CODE                                             
021200                                                                          
021300     PERFORM A-INIT                                                       
021400     PERFORM S01-LAES-LOGMEM                                              
021500     IF NOT END-OF-LOGMEM                                                 
021600       PERFORM UNTIL END-OF-LOGMEM                                        
021700         PERFORM B-STOPPA-IN-I-TABELL                                     
021800         PERFORM S01-LAES-LOGMEM                                          
021900       END-PERFORM                                                        
022000       PERFORM C-BERAEKNA-SLUT                                            
022100       CALL ISPLINK USING ISP-TBDISPL TABELL WPANEL ZERRMSG               
022200       PERFORM UNTIL RETURN-CODE > 4                                      
022300         MOVE SPACE TO ZERRMSG                                            
022310         IF ZTDSELS > 0                                                   
022320           PERFORM UNTIL ZTDSELS = 0                                      
022400             PERFORM D-NYA-VARIABLER                                      
022500             PERFORM E-BROWSA-LISTA                                       
022501             IF ZTDSELS = 1                                               
022502               MOVE ZERO TO ZTDSELS                                       
022503               CALL ISPLINK USING ISP-VPUT N-ZTDSELS                      
022504             ELSE                                                         
022505               CALL ISPLINK USING ISP-TBDISPL TABELL                      
022506             END-IF                                                       
022510           END-PERFORM                                                    
022511         END-IF                                                           
022520         CALL ISPLINK USING ISP-TBDISPL TABELL WPANEL ZERRMSG             
022700       END-PERFORM                                                        
022800     END-IF                                                               
022900                                                                          
023000     PERFORM Z-FINIT                                                      
023100     GOBACK                                                               
023200     .                                                                    
023300     EJECT                                                                
023400 A-INIT SECTION.                                                          
023500     SKIP2                                                                
025808     CALL ISPLINK USING ISP-VDEFINE N-S S CHAR                            
025809                         L-S VDEFINE-OPT                                  
025810     IF RETURN-CODE > 8                                                   
025811       DISPLAY 'KAN INTE VDEFFA S'                                        
025812       PERFORM S99-ABEND                                                  
025813     END-IF                                                               
025814                                                                          
025815     CALL ISPLINK USING ISP-VDEFINE N-LDATE LDATE CHAR                    
025816                         L-LDATE VDEFINE-OPT                              
025817     IF RETURN-CODE > 8                                                   
025818       DISPLAY 'KAN INTE VDEFFA LDATE'                                    
025819       PERFORM S99-ABEND                                                  
025820     END-IF                                                               
025821                                                                          
025822     CALL ISPLINK USING ISP-VDEFINE N-USERID LUSERID CHAR                 
025823                         L-USERID VDEFINE-OPT                             
025824     IF RETURN-CODE > 8                                                   
025825       DISPLAY 'KAN INTE VDEFFA LUSERID'                                  
025826       PERFORM S99-ABEND                                                  
025827     END-IF                                                               
025828                                                                          
025829     CALL ISPLINK USING ISP-VDEFINE N-LSTART LSTART CHAR                  
025830                         L-LSTART VDEFINE-OPT                             
025831     IF RETURN-CODE > 8                                                   
025832       DISPLAY 'KAN INTE VDEFFA LSTART'                                   
025833       PERFORM S99-ABEND                                                  
025834     END-IF                                                               
025835                                                                          
025836     CALL ISPLINK USING ISP-VDEFINE N-LMAX LMAX CHAR                      
025837                         L-LMAX VDEFINE-OPT                               
025838     IF RETURN-CODE > 8                                                   
025839       DISPLAY 'KAN INTE VDEFFA LMAX'                                     
025840       PERFORM S99-ABEND                                                  
025841     END-IF                                                               
025842                                                                          
025843     CALL ISPLINK USING ISP-VDEFINE N-ZERRMSG ZERRMSG CHAR                
025844                         L-ZERRMSG VDEFINE-OPT                            
025845     IF RETURN-CODE > 8                                                   
025846       DISPLAY 'KAN INTE VDEFFA ZERRMSG'                                  
025850       PERFORM S99-ABEND                                                  
025860     END-IF                                                               
025870                                                                          
025880     CALL ISPLINK USING ISP-VDEFINE N-ZTDTOP ZTDTOP CHAR                  
025890                         L-ZTDTOP VDEFINE-OPT                             
025891     IF RETURN-CODE > 8                                                   
025892       DISPLAY 'KAN INTE VDEFFA ZTDTOP'                                   
025893       PERFORM S99-ABEND                                                  
025894     END-IF                                                               
025895                                                                          
025896     CALL ISPLINK USING ISP-VDEFINE N-ZTDSELS ZTDSELS CHAR                
025897                         L-ZTDSELS VDEFINE-OPT                            
025898     IF RETURN-CODE > 8                                                   
025899       DISPLAY 'KAN INTE VDEFFA ZTDSELS'                                  
025900       PERFORM S99-ABEND                                                  
025901     END-IF                                                               
025902                                                                          
025910     CALL ISPLINK USING ISP-VDEFINE N-DDVAR DDVAR CHAR                    
026000                         L-DDVAR VDEFINE-OPT                              
026100     IF RETURN-CODE > 8                                                   
026200       DISPLAY 'KAN INTE VDEFFA DDVAR'                                    
026300       PERFORM S99-ABEND                                                  
026400     END-IF                                                               
026500                                                                          
026600     CALL ISPLINK USING ISP-VDEFINE N-LOGLIB LOGLIB CHAR                  
026700                         L-LOGLIB VDEFINE-OPT                             
026800     IF RETURN-CODE > 0                                                   
026900       DISPLAY 'SAKNAR NAMN PÅ LOGBIBLIOTEK'                              
027000       PERFORM S99-ABEND                                                  
027100     END-IF                                                               
027200                                                                          
027300     CALL ISPLINK USING ISP-VDEFINE N-LOGMEM LOGMEM CHAR                  
027400                         L-LOGMEM VDEFINE-OPT                             
027500     IF RETURN-CODE > 0                                                   
027600       DISPLAY 'SAKNAR NAMN PÅ LOGMEDLEM'                                 
027700       PERFORM S99-ABEND                                                  
027800     END-IF                                                               
027900                                                                          
028000     CALL ISPLINK USING ISP-VDEFINE N-LIBRECFM LIBRECFM CHAR              
028100                         L-LIBRECFM VDEFINE-OPT                           
028200     IF RETURN-CODE > 8                                                   
028300       DISPLAY 'KAN INTE VDEFFA LIBRECFM'                                 
028400       PERFORM S99-ABEND                                                  
028500     END-IF                                                               
028600                                                                          
028700     CALL ISPLINK USING ISP-VDEFINE N-LIBLRECL LIBLRECL CHAR              
028800                         L-LIBLRECL VDEFINE-OPT                           
028900     IF RETURN-CODE > 8                                                   
029000       DISPLAY 'KAN INTE VDEFFA LIBLRECL'                                 
029100       PERFORM S99-ABEND                                                  
029200     END-IF                                                               
029300                                                                          
029400     CALL ISPLINK USING ISP-VDEFINE N-RADLRECL RADLRECL PACK              
029500                         L-RADLRECL VDEFINE-OPT                           
029600     IF RETURN-CODE > 8                                                   
029700       DISPLAY 'KAN INTE VDEFFA RADLRECL'                                 
029800       PERFORM S99-ABEND                                                  
029900     END-IF                                                               
030000                                                                          
030100     CALL ISPLINK USING ISP-VDEFINE N-MEMPOST MEMPOST CHAR                
030200                         L-MEMPOST VDEFINE-OPT                            
030300     IF RETURN-CODE > 8                                                   
030400       DISPLAY 'KAN INTE VDEFFA MEMPOST'                                  
030500       PERFORM S99-ABEND                                                  
030600     END-IF                                                               
041200                                                                          
041300     MOVE SPACE TO WDSNAME                                                
041400     STRING '''' LOGLIB '.' LOGMEM ''''                                   
041500         DELIMITED BY SPACE INTO WDSNAME                                  
041600                                                                          
041700     CALL ISPLINK USING LMINIT N-DDVAR WDUMMY WDUMMY WDUMMY               
041800                        WDUMMY WDUMMY WDUMMY WDSNAME WDUMMY               
041900                        WDUMMY WDUMMY ISP-SHR                             
042000     IF RETURN-CODE > 0                                                   
042100       DISPLAY 'LMINIT RETURN CODE: ' RETURN-CODE                         
042200       PERFORM S99-ABEND                                                  
042300     END-IF                                                               
042400                                                                          
042500     CALL ISPLINK USING LMOPEN DDVAR ISP-INPUT N-LIBLRECL                 
042600                        N-LIBRECFM WDUMMY                                 
042700     IF RETURN-CODE > 0                                                   
042810       DISPLAY 'LMOPEN RETURN CODE: ' RETURN-CODE                         
042900       PERFORM S99-ABEND                                                  
043000     END-IF                                                               
043100                                                                          
043200     EVALUATE TRUE                                                        
043300                                                                          
043400       WHEN LIBRECFM = 'F' OR 'FB'                                        
043500         MOVE 'F' TO RECFM                                                
043600                                                                          
043700       WHEN LIBRECFM = 'FA' OR 'FBA'                                      
043800         MOVE 'FA' TO RECFM                                               
043900                                                                          
044000       WHEN LIBRECFM = 'V' OR 'VB'                                        
044100         MOVE 'V' TO RECFM                                                
044200                                                                          
044300       WHEN LIBRECFM = 'VA' OR 'VBA'                                      
044400         MOVE 'VA' TO RECFM                                               
044500                                                                          
044600       WHEN OTHER                                                         
044700         DISPLAY 'RECORD FORMAT ' LIBRECFM ' IS NOT SUPPORTED.'           
044800         PERFORM S99-ABEND                                                
044900     END-EVALUATE                                                         
045000                                                                          
045100     IF LIBLRECL = SPACE                                                  
045200       DISPLAY 'SAKNAR POSTLÄNGD PÅ LISTBIBLIOTEK'                        
045300       PERFORM S99-ABEND                                                  
045400     END-IF                                                               
045500                                                                          
045600     MOVE LIBLRECL TO DEC-IDFRIDATA                                       
045700     MOVE 3        TO DEC-KVHELTAL                                        
045800     MOVE 0        TO DEC-KVDECIMAL                                       
045900                                                                          
046000     CALL WDECEDIT USING DEC-WDECAREA                                     
046100                                                                          
046200     IF DEC-KDSVAR-FEL                                                    
046300       DISPLAY 'FEL FRÅN WDECEDIT'                                        
046400       PERFORM S99-ABEND                                                  
046500     END-IF                                                               
046600                                                                          
046700     MOVE DEC-IDEDITDATA TO LRECL MAXLRECL RADLRECL                       
046800     IF LRECL > 150                                                       
046900       DISPLAY 'LMAX POSTLÄNGD 150'                                       
047000       PERFORM S99-ABEND                                                  
047100     END-IF                                                               
047200                                                                          
047300     MOVE 0 TO TAB-IX                                                     
047400     MOVE ZERO TO READPTR DATAPTR                                         
047800                                                                          
047900     MOVE SPACE TO RADTAB                                                 
047910                                                                          
047920     CALL ISPLINK USING ISP-TBCREATE TABELL TAB-KEYS TAB-NAMES            
047930                        ISP-NOWRITE ISP-REPLACE WDUMMY ISP-SHARE          
047940     IF RETURN-CODE > 0                                                   
047950       DISPLAY 'TBCREATE RETURN CODE: ' RETURN-CODE                       
047960       PERFORM S99-ABEND                                                  
047970     END-IF                                                               
048000     .                                                                    
048100     EJECT                                                                
048200 B-STOPPA-IN-I-TABELL  SECTION.                                           
048300     SKIP2                                                                
048400     IF TAB-IX = 20000                                                    
048500     DISPLAY 'FULL TABELL. MAX 20000 RADER. ÖKA OCH KOMPILERA OM.'        
048600       PERFORM S99-ABEND                                                  
048700     END-IF                                                               
048800                                                                          
049010     IF MEMPOST-ID = 'NEWLOG'                                             
049020       MOVE SPACE TO S                                                    
049021       MOVE ZERO  TO LMAX                                                 
049030       MOVE MEMPOST-AADDD TO LDATE                                        
049040       MOVE MEMPOST-USERID TO LUSERID                                     
049050       MOVE TAB-IX TO LSTART                                              
049051       CALL ISPLINK USING ISP-TBADD TABELL                                
049053       IF RETURN-CODE > 0                                                 
049054         DISPLAY 'TBADD RETURN CODE: ' RETURN-CODE                        
049055         PERFORM S99-ABEND                                                
049056       END-IF                                                             
049057     ELSE                                                                 
049058       ADD +1 TO TAB-IX                                                   
049059       MOVE MEMPOST TO DATARAD (TAB-IX)                                   
049061     END-IF                                                               
049070     .                                                                    
092600     EJECT                                                                
092700 C-BERAEKNA-SLUT  SECTION.                                                
092800     SKIP2                                                                
094100     CALL ISPLINK USING ISP-TBBOTTOM TABELL                               
094200     IF RETURN-CODE > 0                                                   
094300       DISPLAY 'TBBOTTOM RETURN CODE: ' RETURN-CODE                       
094400       PERFORM S99-ABEND                                                  
094500     END-IF                                                               
094520     MOVE TAB-IX TO OLD-LSTART                                            
094600     PERFORM UNTIL RETURN-CODE > 0                                        
094605       MOVE OLD-LSTART TO LMAX                                            
094606       CALL ISPLINK USING ISP-TBMOD TABELL                                
094607       IF RETURN-CODE > 0                                                 
094608         DISPLAY 'TBMOD RETURN CODE: ' RETURN-CODE                        
094609         PERFORM S99-ABEND                                                
094610       END-IF                                                             
094611       MOVE LSTART TO OLD-LSTART                                          
094612       CALL ISPLINK USING ISP-TBSKIP TABELL TABSKIP                       
094613     END-PERFORM                                                          
094614                                                                          
094620     MOVE SPACE TO S                                                      
094630     CALL ISPLINK USING ISP-VPUT N-S                                      
094640     CALL ISPLINK USING ISP-VPUT N-RADLRECL                               
094700     .                                                                    
094800     EJECT                                                                
113100 D-NYA-VARIABLER SECTION.                                                 
113200     SKIP2                                                                
113201     CALL ISPLINK USING ISP-VDEFINE N-TOPRADNR TOPRADNR PACK              
113202                         L-TOPRADNR VDEFINE-OPT                           
113203     IF RETURN-CODE > 8                                                   
113204       DISPLAY 'KAN INTE VDEFFA TOPRADNR'                                 
113205       PERFORM S99-ABEND                                                  
113206     END-IF                                                               
113207                                                                          
113208     CALL ISPLINK USING ISP-VDEFINE N-MAXRADNR MAXRADNR PACK              
113209                         L-MAXRADNR VDEFINE-OPT                           
113210     IF RETURN-CODE > 8                                                   
113211       DISPLAY 'KAN INTE VDEFFA MAXRADNR'                                 
113212       PERFORM S99-ABEND                                                  
113213     END-IF                                                               
113214                                                                          
113215     MOVE LSTART TO TOPRADNR                                              
113220     COMPUTE MAXRADNR = LMAX - TOPRADNR                                   
113300     MOVE SPACE TO S                                                      
113400     CALL ISPLINK USING ISP-VPUT N-TOPRADNR                               
113500     CALL ISPLINK USING ISP-VPUT N-MAXRADNR                               
113600     CALL ISPLINK USING ISP-VPUT N-S                                      
113700     .                                                                    
113800     EJECT                                                                
123100 E-BROWSA-LISTA  SECTION.                                                 
123200     SKIP2                                                                
123300     CALL W9600610 USING READPTR DATAPTR RADTAB                           
123400                                                                          
123500     CALL ISPLINK USING ISP-CONTROL ISP-ERRORS ISP-RETURN                 
123600     CALL ISPLINK USING ISP-BRIF WDUMMY RECFM LRECL                       
123700           READPTR WDUMMY DATAPTR                                         
123800                                                                          
123900     IF RETURN-CODE > 0                                                   
124000       IF RETURN-CODE = 12                                                
124100         CONTINUE                                                         
124200       ELSE                                                               
124300         DISPLAY 'RETURKOD STÖRRE ÄN 12 EFTER BRIF'                       
124400         PERFORM S99-ABEND                                                
124500       END-IF                                                             
124600     END-IF                                                               
124700     CALL ISPLINK USING ISP-CONTROL ISP-ERRORS ISP-CANCEL                 
124800                                                                          
124900     CANCEL W9600620                                                      
125100                                                                          
125200     CALL WDELETE USING W9600620                                          
125400                                                                          
125700     CALL ISPLINK USING ISP-VDELETE N-TOPRADNR                            
125800     CALL ISPLINK USING ISP-VDELETE N-MAXRADNR                            
125900     .                                                                    
126000     EJECT                                                                
126100 Z-FINIT SECTION.                                                         
126200     SKIP2                                                                
126300     CALL ISPLINK USING ISP-TBEND TABELL                                  
126310     CALL ISPLINK USING ISP-VRESET                                        
126400                                                                          
126500     CALL ISPLINK USING LMCLOSE DDVAR                                     
126600     CALL ISPLINK USING LMFREE  DDVAR                                     
126700     .                                                                    
126800     EJECT                                                                
126900 S01-LAES-LOGMEM   SECTION.                                               
127000     SKIP2                                                                
127100     CALL ISPLINK USING LMGET DDVAR ISP-INVAR N-MEMPOST                   
127200                        N-VARLRECL MAXLRECL                               
127300     IF RETURN-CODE > 0                                                   
127400       IF RETURN-CODE = 8                                                 
127500         SET END-OF-LOGMEM TO TRUE                                        
127600       ELSE                                                               
127700         DISPLAY 'LMGET RETURKOD ' RETURN-CODE                            
127800         PERFORM S99-ABEND                                                
127900       END-IF                                                             
128000     END-IF                                                               
128100     .                                                                    
128200 S99-ABEND SECTION.                                                       
128300                                                                          
128400     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
128500     .                                                                    
