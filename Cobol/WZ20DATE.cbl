000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ20DATE.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   02/04/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*                                                                         
000900*    1. CHECKS IF A DATE IS VALID AND CONVERTS IT TO                      
001000*       LILIAN FORMAT (NBR OF DAYS SINCE OCTOBER 15, 1582)                
001100*    2. CONVERTS A DATE IN LILIAN FORMAT TO SOME OTHER FORMAT.            
001200*                                                                         
001300*    THE DATE MAY BE GIVEN/RETURNED IN MANY DIFFERENT FORMATS.            
001400*    COBOL LANGUAGE ENVIRONMENT STANDARD PROGRAM "CEEDATE"                
001500*    IS USED FOR VALIDATING AND CONVERTING THE DATE IN MOST               
001600*    CASES, SO ALL FORMATS SUPPORTED BY THIS SUBROUTINE IS                
001700*    ALSO SUPPORTED HERE. BESIDE THOSE FORMATS, WEEK NUMBER ("WW")        
001800*    AND DAY-IN-WEEK (A SINGLE "D") ARE ALSO SUPPORTED.                   
001900*                                                                         
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200     SKIP3                                                                
002300 WORKING-STORAGE SECTION.                                                 
002400     EJECT                                                                
002500 01  CEEDAYS         PIC X(8) VALUE 'CEEDAYS'.                            
002600 01  CEEDATE         PIC X(8) VALUE 'CEEDATE'.                            
002700                                                                          
002800 01  W-TIDATE-IN.                                                         
002900     03 W-TIDATE-L   PIC S9(4) BINARY.                                    
003000     03 W-TIDATE     PIC X(30).                                           
003100                                                                          
003200 01  W-TIDATE-OUT    PIC X(80).                                           
003300                                                                          
003400 01  W-KDDATFMT-G.                                                        
003500     03 W-KDDATFMT-L PIC S9(4) BINARY.                                    
003600     03 W-KDDATFMT   PIC X(30).                                           
003700                                                                          
003800 01  W-TILILDAT      PIC 9(9) BINARY.                                     
003900                                                                          
004000 01  FBC.                                                                 
004100     03 SEV          PIC S9(4) BINARY.                                    
004200     03 MSGNO        PIC S9(4) BINARY.                                    
004300     03 FILLER       PIC X(8).                                            
004400                                                                          
004700 01  W-TEMP          PIC S9(9) BINARY.                                    
004710 01  SLASK           PIC S9(9) BINARY.                                    
004720 01  DDD-TEMP        PIC S9(3) PACKED-DECIMAL.                            
004800                                                                          
004900 01  JAN01-DATUM.                                                         
005000     03 FILLER       PIC S9(4) BINARY VALUE +7.                           
005100     03 JAN01-YEAR   PIC X(4).                                            
005200     03 FILLER       PIC X(3)  VALUE '001'.                               
005300                                                                          
005400 01  JAN01-LILIAN    PIC S9(9) BINARY.                                    
005500                                                                          
005600 01  JAN01-WEEKDAY-FORMAT.                                                
005700     03 FILLER       PIC S9(4) BINARY VALUE +3.                           
005800     03 FILLER       PIC X(3)  VALUE 'WWW'.                               
005900                                                                          
006000 01  JAN01-WEEKDAY   PIC X(80).                                           
006100                                                                          
006200*    -- TABLE USED TO COMPUTE OFFSET TO FIRST DAY OF WEEK 01              
006300*    -- DEPENING ON THE WEEKDAY OF JAN 01.                                
006400 01  WEEKDAY-LIST    PIC X(21) VALUE 'MONTUEWEDTHUFRISATSUN'.             
006500 01  WEEKDAY-OFFS.                                                        
006600     03  MON-OFF     PIC S9(3)  VALUE +0.                                 
006700     03  TUE-OFF     PIC S9(3)  VALUE -1.                                 
006800     03  WED-OFF     PIC S9(3)  VALUE -2.                                 
006900     03  THU-OFF     PIC S9(3)  VALUE -3.                                 
007000     03  FRI-OFF     PIC S9(3)  VALUE +3.                                 
007100     03  SAT-OFF     PIC S9(3)  VALUE +2.                                 
007200     03  SUN-OFF     PIC S9(3)  VALUE +1.                                 
007300 01  JAN01-OFF-G.                                                         
007400     03  JAN01-OFF   PIC S9(3).                                           
007500                                                                          
007600 01  JULIAN-DATUM.                                                        
007700     03 FILLER       PIC S9(4) BINARY VALUE +7.                           
007800     03 JULIAN-YEAR  PIC X(4).                                            
007900     03 DDD          PIC 9(3).                                            
008000                                                                          
008100 01  JULIAN-DATUM-OUT PIC X(80).                                          
008200                                                                          
008300 01  JULIAN-FORMAT.                                                       
008400     03 FILLER       PIC S9(4) BINARY VALUE +7.                           
008500     03 JULIAN-YEAR-FMT                                                   
008600                     PIC X(4).                                            
008700     03 FILLER       PIC X(3)  VALUE 'DDD'.                               
008800                                                                          
008900*    -- FOR KEEPING TRACK OF YEAR, WEEK AND DAY IN FORMAT STRING          
009000*    -- AND COMPUTING JAN-01 OF THE YEAR AND JULIAN DATE FROM WEEK        
009100 01  Y4              PIC X(4)  VALUE 'YYYY'.                              
009200 01  YSTART          PIC S9(4) BINARY.                                    
009300 01  YLENGTH         PIC S9(4) BINARY.                                    
009400 01  WSTART          PIC S9(4) BINARY.                                    
009500 01  WLENGTH         PIC S9(4) BINARY.                                    
009600 01  DSTART          PIC S9(4) BINARY.                                    
009700 01  DLENGTH         PIC S9(4) BINARY.                                    
009800                                                                          
009900*    -- WORK FIELDS WHEN COMPUTING WEEK AND DAY OF WEEK                   
009910*    -- THE LISTED YEARS ARE THOSE THAT HAVE 53 WEEKS                     
010000 01  YYYY            PIC 9(4).                                            
010010 88  W53-YEAR        VALUE 1987, 1992, 1998, 2004, 2009,                  
010020                           2015, 2020, 2026, 2032, 2037.                  
010100 01  WW              PIC 9(2).                                            
010200 01  D               PIC 9.                                               
010300                                                                          
010400     EJECT                                                                
010500 LINKAGE SECTION.                                                         
010600                                                                          
010700*01 -COPY WZ20DATE                                                        
010800                                                                          
010900*                                                                         
011000 PROCEDURE DIVISION USING DATE-WZ20DATE.                                  
011100                                                                          
011200     PERFORM A-CHECK-PREPARE                                              
011300                                                                          
011400     IF DATE-TIDATE = SPACE                                               
011500       PERFORM B-LILIAN-TO-DATE                                           
011600     ELSE                                                                 
011700       PERFORM C-DATE-TO-LILIAN                                           
011800     END-IF                                                               
011900                                                                          
012000     GOBACK                                                               
012100     .                                                                    
012200     EJECT                                                                
012300                                                                          
012400 A-CHECK-PREPARE   SECTION.                                               
012500                                                                          
012600*    -- NO ERROR YET                                                      
012700     MOVE ZERO TO DATE-KDRC                                               
012800                                                                          
012900*    -- INPECT DATE ARGUMENT AND PREPARE FOR CEEDAYS                      
012910     IF DATE-TIDATE NOT = SPACE                                           
013000       MOVE ZERO TO TALLY                                                 
013100       INSPECT DATE-TIDATE  TALLYING TALLY                                
013200               FOR CHARACTERS BEFORE INITIAL '  '                         
013201       MOVE TALLY TO W-TIDATE-L                                           
013400         MOVE DATE-TIDATE(1:TALLY) TO W-TIDATE                            
013401     ELSE                                                                 
013402*      -- NOT REALLY USED, BUT LET'S DO IT ANYWAY                         
013403       MOVE ZERO  TO W-TIDATE-L                                           
013404       MOVE SPACE TO W-TIDATE                                             
013410     END-IF                                                               
013500                                                                          
013600*    -- INPECT FORMAT ARGUMENT AND PREPARE FOR CEEDATE/CEEDAYS            
013700     MOVE ZERO TO TALLY                                                   
013800     INSPECT DATE-KDDATFMT TALLYING TALLY                                 
013900             FOR CHARACTERS BEFORE INITIAL '  '                           
014000     MOVE TALLY TO W-KDDATFMT-L                                           
014100     MOVE DATE-KDDATFMT(1:TALLY) TO W-KDDATFMT                            
014200     .                                                                    
014300     EJECT                                                                
014400                                                                          
014500 B-LILIAN-TO-DATE  SECTION.                                               
014600                                                                          
014700*    -- COMPUTE DAY NUMBER (MON=1, TUE=2 ... ) FOR LATER USE              
014800     COMPUTE W-TEMP   = DATE-TILILDAT + 3                                 
014910     COMPUTE D        = FUNCTION REM(W-TEMP, 7) + 1                       
015000                                                                          
015010*    -- CHECK IF FORMAT CONTAINS YEAR NUMBER.                             
015030     MOVE 1 TO YSTART                                                     
015040     INSPECT DATE-KDDATFMT TALLYING YSTART                                
015050             FOR CHARACTERS BEFORE INITIAL 'Y'                            
015060     MOVE ZERO TO YLENGTH                                                 
015070     INSPECT DATE-KDDATFMT(YSTART:) TALLYING YLENGTH                      
015080             FOR LEADING 'Y'                                              
015090                                                                          
015100*    -- CHECK IF FORMAT CONTAINS WEEK NUMBER.                             
015200     MOVE 1 TO WSTART                                                     
015300     INSPECT DATE-KDDATFMT TALLYING WSTART                                
015400     FOR CHARACTERS BEFORE INITIAL 'W'                                    
015500     MOVE ZERO TO WLENGTH                                                 
015600     IF WSTART <= 20                                                      
015700       INSPECT DATE-KDDATFMT(WSTART:) TALLYING WLENGTH                    
015800       FOR LEADING 'W'                                                    
015900     END-IF                                                               
016000                                                                          
016100     IF WSTART > 20 OR WLENGTH NOT = 2                                    
016200*      -- NO WEEK NUMBER IN DATE, JUST CONVERT LILIAN TO TEXT             
016300       CALL CEEDATE USING DATE-TILILDAT W-KDDATFMT-G                      
016400                          W-TIDATE-OUT FBC                                
016500     ELSE                                                                 
016600*      -- DATE CONTAINS A WEEK NUMBER. SINCE THIS IS NOT SUPPORTED        
016700*      -- BY CEEDAYS, COMPUTE JULIAN DAY ("DDD") AND DAY OF               
016800*      -- WEEK ("D") AND USE THIS TO COMPUTE THE WEEK                     
016900                                                                          
017000*      -- FIRST CONVERT TO TEXT WITHOUT THE WEEK NUMBER                   
017100*      MOVE SPACE TO W-KDDATFMT(WSTART:2)                                 
017200       CALL CEEDATE USING DATE-TILILDAT W-KDDATFMT-G                      
017300                          W-TIDATE-OUT FBC                                
017400                                                                          
017500       MOVE Y4 TO JULIAN-YEAR-FMT                                         
017600       CALL CEEDATE USING DATE-TILILDAT JULIAN-FORMAT                     
017700                          JULIAN-DATUM-OUT FBC                            
017800       MOVE JULIAN-DATUM-OUT(1:4) TO YYYY                                 
017900       MOVE JULIAN-DATUM-OUT(5:3) TO DDD                                  
017901                                                                          
017910*      -- GET LILIAN OF JAN-01 THE SAME YEAR                              
017911       MOVE YYYY TO JAN01-YEAR                                            
017920       CALL CEEDAYS USING JAN01-DATUM JULIAN-FORMAT                       
017930                          JAN01-LILIAN FBC                                
017940*      -- GET WEEKDAY OF JAN 01 (MON, TUE, WED ... )                      
017950       CALL CEEDATE USING JAN01-LILIAN JAN01-WEEKDAY-FORMAT               
017960                          JAN01-WEEKDAY FBC                               
017970*      -- CONVERT IT TO AN OFFSET TO FIRST DAY OF WEEK                    
017980       MOVE 1 TO TALLY                                                    
017990       INSPECT WEEKDAY-LIST TALLYING TALLY FOR CHARACTERS                 
017991               BEFORE INITIAL JAN01-WEEKDAY(1:3)                          
017992       MOVE WEEKDAY-OFFS(TALLY:3) TO JAN01-OFF-G                          
018000                                                                          
018100*      -- FINALLY COMPUTE WEEK NUMBER FOR NORMAL WEEKS                    
018110       COMPUTE WW = (DDD - D + 7 - JAN01-OFF) / 7                         
018200                                                                          
018300*      -- SPECIAL YEAR-END WEEK LOGIC                                     
018500       IF WW = 0 OR 53                                                    
018600         IF WW = 0                                                        
018800           SUBTRACT 1 FROM YYYY                                           
018810           IF W53-YEAR                                                    
018820             MOVE 53 TO WW                                                
018821           ELSE                                                           
018822             MOVE 52 TO WW                                                
018830           END-IF                                                         
018900         END-IF                                                           
019000         IF WW = 53 AND NOT W53-YEAR                                      
019030*          -- KEEP WEEK 53 FOR CERTAIN YEARS                              
019100           MOVE 1 TO WW                                                   
019200           ADD 1 TO YYYY                                                  
019300         END-IF                                                           
020300         IF YSTART <= 20 AND YLENGTH >= 1 AND <= 4                        
020400           STRING YYYY(5 - YLENGTH: YLENGTH) DELIMITED BY SIZE            
020500                  INTO W-TIDATE-OUT WITH POINTER YSTART                   
020600         END-IF                                                           
020700       END-IF                                                             
020800                                                                          
020900*      -- INSERT THE WEEK NUMBER AT THE CORRECT POSITION                  
021000*      -- IN THE OUTPUT DATE                                              
021100       STRING WW DELIMITED BY SIZE                                        
021200              INTO  W-TIDATE-OUT WITH POINTER WSTART                      
021300                                                                          
021400     END-IF                                                               
021500                                                                          
021600*    -- CHECK IF FORMAT CONTAINS DAY-OF WEEK (1 DIGIT DAY)                
021700     MOVE 1 TO DSTART                                                     
021800     INSPECT DATE-KDDATFMT TALLYING DSTART                                
021900     FOR CHARACTERS BEFORE INITIAL 'D'                                    
022000     MOVE ZERO TO DLENGTH                                                 
022100     IF DSTART <= 20                                                      
022200       INSPECT DATE-KDDATFMT(DSTART:) TALLYING DLENGTH                    
022300       FOR LEADING 'D'                                                    
022400     END-IF                                                               
022500     IF DSTART <= 20 AND DLENGTH = 1                                      
022700*        -- INSERT DAY OF WEEK NUMBER AT THE CORRECT POSITION             
022800*        -- IN THE OUTPUT DATE                                            
022900       STRING D DELIMITED BY SIZE INTO W-TIDATE-OUT                       
023000              WITH POINTER DSTART                                         
023100     END-IF                                                               
023200                                                                          
023300     MOVE W-TIDATE-OUT TO DATE-TIDATE                                     
023400     IF SEV > 0                                                           
023500       MOVE 8 TO DATE-KDRC                                                
023600     END-IF                                                               
023700     .                                                                    
023800     EJECT                                                                
023900                                                                          
024000 C-DATE-TO-LILIAN  SECTION.                                               
024100                                                                          
024200*    -- CHECK IF FORMAT CONTAINS YEAR NUMBER. APPEND IF NOT               
024300     MOVE 1 TO TALLY                                                      
024400     INSPECT DATE-KDDATFMT TALLYING TALLY                                 
024500     FOR CHARACTERS BEFORE INITIAL 'Y'                                    
024600     IF TALLY > 20                                                        
024700*      -- NO YEAR IN DATE - PROVIDE CURRENT YEAR                          
024800       STRING '-' FUNCTION CURRENT-DATE(1:4)                              
024900       DELIMITED BY SIZE INTO W-TIDATE(W-TIDATE-L + 1:)                   
025000       COMPUTE YSTART = W-TIDATE-L + 2                                    
025100       MOVE 4 TO YLENGTH                                                  
025200       ADD 5 TO W-TIDATE-L                                                
025300       STRING '-YYYY'                                                     
025400       DELIMITED BY SIZE INTO W-KDDATFMT(W-KDDATFMT-L + 1:)               
025500       ADD 5 TO W-KDDATFMT-L                                              
025600     ELSE                                                                 
025700       MOVE TALLY TO YSTART                                               
025800       MOVE ZERO  TO YLENGTH                                              
025900       INSPECT W-KDDATFMT(YSTART:)                                        
026000               TALLYING YLENGTH FOR LEADING 'Y'                           
026100     END-IF                                                               
026200     MOVE W-TIDATE(YSTART:YLENGTH) TO JAN01-YEAR                          
026300     MOVE Y4(1:YLENGTH)           TO JULIAN-YEAR-FMT                      
026400                                                                          
026500*    -- CHECK IF FORMAT CONTAINS DAY NUMBER. APPEND IF NOT                
026600     MOVE 1 TO DSTART                                                     
026700     INSPECT DATE-KDDATFMT TALLYING DSTART                                
026800     FOR CHARACTERS BEFORE INITIAL 'D'                                    
026900     MOVE ZERO TO DLENGTH                                                 
027000     IF DSTART > 20                                                       
027100*      -- NO DAYS IN DATE - PROVIDE '01'                                  
027200       STRING '-01'                                                       
027300       DELIMITED BY SIZE INTO W-TIDATE(W-TIDATE-L + 1:)                   
027400       ADD 3 TO W-TIDATE-L                                                
027500       STRING '-DD'                                                       
027600       DELIMITED BY SIZE INTO W-KDDATFMT(W-KDDATFMT-L + 1:)               
027700       ADD 3 TO W-KDDATFMT-L                                              
027800     ELSE                                                                 
027900*      -- CHECK HOW MANY D:S IN A ROW TO MAKE IT POSSIBLE TO              
028000*      -- DIFFERENTIATE BETWEEN D, DD AND DDD                             
028100       INSPECT DATE-KDDATFMT(DSTART:) TALLYING DLENGTH                    
028200       FOR LEADING 'D'                                                    
028300     END-IF                                                               
028400                                                                          
028500*    -- CHECK IF FORMAT CONTAINS WEEK NUMBER.                             
028600     MOVE 1 TO WSTART                                                     
028700     INSPECT DATE-KDDATFMT TALLYING WSTART                                
028800     FOR CHARACTERS BEFORE INITIAL 'W'                                    
028900     MOVE ZERO TO WLENGTH                                                 
029000     IF WSTART <= 20                                                      
029100       INSPECT DATE-KDDATFMT(WSTART:) TALLYING WLENGTH                    
029200       FOR LEADING 'W'                                                    
029300     END-IF                                                               
029400                                                                          
029500     IF WSTART > 20 OR WLENGTH NOT = 2                                    
029600*      -- NO WEEK NUMBER IN DATE, JUST COMPUTE THE LILIAN                 
029700       CALL CEEDAYS USING W-TIDATE-IN W-KDDATFMT-G W-TILILDAT FBC         
029800     ELSE                                                                 
029900*      -- DATE CONTAINS A WEEK NUMBER. SINCE THIS IS NOT SUPPORTED        
030000*      -- BY CEEDAYS, IT MUST BE CONVERTED TO JULIAN DAY NBR              
030100*      -- ("DDD") WHICH IS THEN USED TO COMPUTE THE LILIAN.               
030200*      -- FIRST GET INFO ABOUT JAN-01 THE SAME YEAR AND COMPUTE           
030300*      -- THE OFFSET OF JAN-01 TO THE BEGINNING OF WEEK 01.               
030400*      -- THEN COMPUTE JULIAN DATE ("DDD") FROM THE WEEK +                
030500*      -- OPTIONAL WEEKDAY + THE COMPUTED OFFSET OF JAN-01.               
030600                                                                          
030700*      -- GET LILIAN OF JAN-01 THE SAME YEAR                              
030800       CALL CEEDAYS USING JAN01-DATUM JULIAN-FORMAT                       
030900                          JAN01-LILIAN FBC                                
031000*      -- GET WEEKDAY OF JAN 01 (MON, TUE, WED ... )                      
031100       CALL CEEDATE USING JAN01-LILIAN JAN01-WEEKDAY-FORMAT               
031200                          JAN01-WEEKDAY FBC                               
031300*      -- CONVERT IT TO AN OFFSET TO FIRST DAY OF WEEK                    
031400       MOVE 1 TO TALLY                                                    
031500       INSPECT WEEKDAY-LIST TALLYING TALLY FOR CHARACTERS                 
031600               BEFORE INITIAL JAN01-WEEKDAY(1:3)                          
031700       MOVE WEEKDAY-OFFS(TALLY:3) TO JAN01-OFF-G                          
031800                                                                          
031900       IF DSTART <= 20 AND DLENGTH = 1                                    
032000*        -- DAY-OF-WEEK  SPECIFIED                                        
032100         MOVE W-TIDATE(DSTART:1) TO D                                     
032200       ELSE                                                               
032300*        -- NO DAY SPECIFIED, USE 1:ST DAY OF WEEK AS DEFAULT             
032400         MOVE 1 TO D                                                      
032500       END-IF                                                             
032600                                                                          
032700       MOVE W-TIDATE(WSTART:2) TO WW                                      
032800       IF WW > ZERO AND < 54                                              
032900         COMPUTE DDD-TEMP = (WW - 1) * 7 + D + JAN01-OFF                  
032902                                                                          
032903         EVALUATE TRUE                                                    
032910         WHEN DDD-TEMP > 365                                              
032911*          -- THIS CAN HAPPEN IF WEEK IS 53                               
032920           COMPUTE SLASK = DDD-TEMP - 365                                 
032921           MOVE 365 TO DDD                                                
032930                                                                          
032940         WHEN DDD-TEMP < 1                                                
032941*          -- THIS CAN HAPPEN IF WEEK 1 STARTS THE PREVIOUS YEAR          
032942           COMPUTE SLASK = DDD-TEMP - 1                                   
032943           MOVE 1   TO DDD                                                
032950                                                                          
032960         WHEN OTHER                                                       
032961           MOVE ZERO TO SLASK                                             
032970           MOVE DDD-TEMP TO DDD                                           
032971                                                                          
032980         END-EVALUATE                                                     
032990                                                                          
033000         MOVE JAN01-YEAR TO JULIAN-YEAR                                   
033100*        -- NOW USE THE COMPUTED JULIAN DATE TO FIND LILIAN               
033200         CALL CEEDAYS USING                                               
033300           JULIAN-DATUM JULIAN-FORMAT W-TILILDAT FBC                      
033400         ADD SLASK TO W-TILILDAT                                          
033500       ELSE                                                               
033600         MOVE 8 TO SEV                                                    
033700         MOVE ZERO TO W-TILILDAT                                          
033800       END-IF                                                             
033900     END-IF                                                               
034000                                                                          
034100     MOVE W-TILILDAT TO DATE-TILILDAT                                     
034200     IF SEV > 0                                                           
034300       MOVE 8 TO DATE-KDRC                                                
034400     END-IF                                                               
034500     .                                                                    
