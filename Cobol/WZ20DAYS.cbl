000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ20DAYS.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   12/04/2002.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*                                                                         
000900*    COMPUTES THE DIFFERENCE BETWEEN TWO DATES OR                         
001000*    A NEW DATE FROM ONE DATE AND A NUMBER OF DAYS.                       
001100*                                                                         
001200*    THE DATE MAY BE GIVEN IN MANY DIFFERENT FORMATS.                     
001300*    COBOL LANGUAGE ENVIRONMENT STANDARD PROGRAM                          
001400*    CEEDATE IS USED FOR VALIDATING AND CONVERTING THE                    
001500*    INPUT DATE.                                                          
001600*                                                                         
001700     EJECT                                                                
001800 DATA DIVISION.                                                           
001900     SKIP3                                                                
002000 WORKING-STORAGE SECTION.                                                 
002100     EJECT                                                                
002200 01  CEEDATE         PIC X(8) VALUE 'CEEDATE'.                            
002201 01  CEEDAYS         PIC X(8) VALUE 'CEEDAYS'.                            
002210 01  WZ20DATE        PIC X(8) VALUE 'WZ20DATE'.                           
002300                                                                          
002510 01  W-TILILDAT      PIC 9(9) BINARY.                                     
002600                                                                          
002700 01  UTFORMAT-G.                                                          
002800     03 UTFORMAT-L   PIC S9(4) BINARY.                                    
002900     03 UTFORMAT     PIC X(20).                                           
003000                                                                          
003100 01  UTDATUM-G.                                                           
003300     03 UTDATUM      PIC X(80).                                           
003400                                                                          
003500 01  FBC.                                                                 
003600     03 SEV          PIC S9(4) BINARY.                                    
003700     03 MSGNO        PIC S9(4) BINARY.                                    
003800     03 FILLER       PIC X(8).                                            
003900                                                                          
003910 01  JAN01-DATUM.                                                         
003920     03 FILLER       PIC S9(4) BINARY VALUE +7.                           
003930     03 YVALUE       PIC X(4).                                            
003940     03 FILLER       PIC X(3)  VALUE '001'.                               
003941                                                                          
003942 01  JULIAN-FORMAT.                                                       
003943     03 FILLER       PIC S9(4) BINARY VALUE +7.                           
003944     03 JFORMAT      PIC X(4).                                            
003945     03 FILLER       PIC X(3)  VALUE 'DDD'.                               
003950                                                                          
003960 01  JAN01-LILIAN    PIC S9(9) BINARY.                                    
003970                                                                          
003980 01  JAN01-WEEKDAY-FORMAT.                                                
003990     03 FILLER       PIC S9(4) BINARY VALUE +3.                           
003991     03 FILLER       PIC X(3)  VALUE 'WWW'.                               
003992                                                                          
003993 01  JAN01-WEEKDAY   PIC X(80).                                           
003994                                                                          
003995*    -- TABLE USED TO COMPUTE OFFSET TO FIRST DAY OF WEEK 01              
003996*    -- DEPENING ON THE WEEKDAY OF JAN 01.                                
003997 01  WEEKDAY-LIST    PIC X(21) VALUE 'MONTUEWEDTHUFRISATSUN'.             
003998 01  WEEKDAY-OFFS.                                                        
003999     03  MON-OFF     PIC S9(3)  VALUE +0.                                 
004000     03  TUE-OFF     PIC S9(3)  VALUE -1.                                 
004001     03  WED-OFF     PIC S9(3)  VALUE -2.                                 
004002     03  THU-OFF     PIC S9(3)  VALUE -3.                                 
004003     03  FRI-OFF     PIC S9(3)  VALUE +3.                                 
004004     03  SAT-OFF     PIC S9(3)  VALUE +2.                                 
004005     03  SUN-OFF     PIC S9(3)  VALUE +1.                                 
004006 01  JAN01-OFF-G.                                                         
004007     03  JAN01-OFF   PIC S9(3).                                           
004008                                                                          
004010 01  WSTART          PIC S9(4) BINARY.                                    
004020 01  WLENGTH         PIC S9(4) BINARY.                                    
004030 01  DSTART          PIC S9(4) BINARY.                                    
004040 01  WW              PIC 9(2).                                            
004050 01  D               PIC 9.                                               
004051 01  DDD             PIC 999.                                             
004060                                                                          
004100     EJECT                                                                
004200*01  -COPY WZ20DATE                                                       
004300     EJECT                                                                
004400 LINKAGE SECTION.                                                         
004500                                                                          
004600*01 -COPY WZ20DAYS                                                        
004700                                                                          
004800*                                                                         
004900 PROCEDURE DIVISION USING DAYS-WZ20DAYS.                                  
005000                                                                          
005100     MOVE ZERO TO DAYS-KDRC                                               
005200                                                                          
005300*    -- COMPUTE DATE1 = DATE2 - KVDAYS                                    
005400     IF DAYS-TIDATE1 = SPACE                                              
005500       MOVE DAYS-TIDATE2 TO DATE-TIDATE                                   
005600       MOVE DAYS-KDDATFMT2 TO DATE-KDDATFMT                               
005700       CALL WZ20DATE USING DATE-WZ20DATE                                  
005800       IF DATE-KDRC >  ZERO                                               
005900         MOVE 8     TO DAYS-KDRC                                          
006200       ELSE                                                               
006210         MOVE SPACE           TO   DATE-TIDATE                            
006400         MOVE DAYS-KDDATFMT1  TO   DATE-KDDATFMT                          
006500         SUBTRACT DAYS-KVDAYS FROM DATE-TILILDAT                          
007000         CALL WZ20DATE USING DATE-WZ20DATE                                
007100         IF DATE-KDRC > 0                                                 
007200           MOVE 8 TO DAYS-KDRC                                            
007300         ELSE                                                             
007400           MOVE DATE-TIDATE TO DAYS-TIDATE1                               
007500         END-IF                                                           
007600       END-IF                                                             
007610       GOBACK                                                             
007700     END-IF                                                               
007800                                                                          
007900*     -- COMPUTE DATE2 = DATE1 + KVDAYS                                   
008000     IF DAYS-TIDATE2 = SPACE                                              
008100       MOVE DAYS-TIDATE1 TO DATE-TIDATE                                   
008200       MOVE DAYS-KDDATFMT1 TO DATE-KDDATFMT                               
008300       CALL WZ20DATE USING DATE-WZ20DATE                                  
008400       IF DATE-KDRC >  ZERO                                               
008500         MOVE 8     TO DAYS-KDRC                                          
008800       ELSE                                                               
008810         MOVE SPACE           TO   DATE-TIDATE                            
008820         MOVE DAYS-KDDATFMT2  TO   DATE-KDDATFMT                          
008830         ADD  DAYS-KVDAYS     TO   DATE-TILILDAT                          
008840         CALL WZ20DATE USING DATE-WZ20DATE                                
008850         IF DATE-KDRC > 0                                                 
009800           MOVE 8 TO DAYS-KDRC                                            
009900         ELSE                                                             
010000           MOVE DATE-TIDATE TO DAYS-TIDATE2                               
010100         END-IF                                                           
010200       END-IF                                                             
010210       GOBACK                                                             
010300     END-IF                                                               
010400                                                                          
010500*     -- COMPUTE KVDAYS = DATE2 - DATE1                                   
010600     IF DAYS-TIDATE1 NOT = SPACE AND DAYS-TIDATE2 NOT = SPACE             
010700       MOVE DAYS-TIDATE1 TO DATE-TIDATE                                   
010800       MOVE DAYS-KDDATFMT1 TO DATE-KDDATFMT                               
010900       CALL WZ20DATE USING DATE-WZ20DATE                                  
011000       IF DATE-KDRC >  ZERO                                               
011100         MOVE 8    TO DAYS-KDRC                                           
011200         MOVE ZERO TO DAYS-KVDAYS                                         
011400       ELSE                                                               
011500         MOVE DATE-TILILDAT TO W-TILILDAT                                 
011510                                                                          
011600         MOVE DAYS-TIDATE2 TO DATE-TIDATE                                 
011700         MOVE DAYS-KDDATFMT2 TO DATE-KDDATFMT                             
011800         CALL WZ20DATE USING DATE-WZ20DATE                                
011900         IF DATE-KDRC >  ZERO                                             
012000           MOVE 8    TO DAYS-KDRC                                         
012100           MOVE ZERO TO DAYS-KVDAYS                                       
012300         ELSE                                                             
012400           COMPUTE DAYS-KVDAYS = DATE-TILILDAT - W-TILILDAT               
012800         END-IF                                                           
012810       END-IF                                                             
012820       GOBACK                                                             
012900     END-IF                                                               
013000                                                                          
013900     GOBACK                                                               
014000     .                                                                    
