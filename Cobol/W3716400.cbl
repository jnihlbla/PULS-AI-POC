000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3716400.                                                
000300 AUTHOR.         GAVIN SMITH.                                             
000400 DATE-WRITTEN.   99/10/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        THIS PROGRAM READS THE POINT TRANSACTION FILE CREATED            
001000*        IN W371V4 (W37164) AND CREATES AN OUTPUT FILE WHICH IS           
001100*        THEN RENAMED BY A NOAC ROUTINE AND SENT TO THE IMPORTER          
001200*        VIPS SYSTEM VIA VCOM.                                            
001300*                                                                         
001400*                                                                         
001500*    ABENDCODES:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002601     SKIP2                                                                
002602*          --- WEEKLY TRANSACTION FILE WITH XCHANGE POINT TRANSACT        
002603     SELECT W37164                     ASSIGN TO W37164D1.                
002604     SKIP2                                                                
002605*          --- WEEKLY POINTREGISTER TO VIPS MARKETS                       
002610     SELECT W37167                     ASSIGN TO W37164D2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003201     SKIP3                                                                
003202 FD  W37164                                                               
003203     RECORDING       F                                                    
003204     BLOCK CONTAINS  0.                                                   
003205                                                                          
003206*01  -COPY W37164      -L.                                                
003207     SKIP3                                                                
003208 FD  W37167                                                               
003209     RECORDING       V                                                    
003210     BLOCK CONTAINS  0.                                                   
003220*01  POST -COPY W37167   -PRE UT-   -L.                                   
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(8)    VALUE 'W3716400'.            
003700 77  YES                         PIC X       VALUE 'Y'.                   
003800 77  NOO                         PIC X       VALUE 'N'.                   
004001 77  COUNTER                     PIC S9(5)   VALUE ZERO.                  
004002 77  W37164-EOF-SW               PIC X       VALUE 'N'.                   
004010     88  END-OF-W37164                       VALUE 'Y'.                   
004100     EJECT                                                                
004200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004300 01  FILLER REDEFINES TODAYS-DATE.                                        
004400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004600     03  TODAYS-DATE-DAY         PIC 9(2).                                
004700     EJECT                                                                
004800 01  GENERAL-SUBPROGRAMS.                                                 
004900*                                                                         
005000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005200     SKIP2                                                                
005300*    --- PARAMETERS TO ABEND                                              
005400                                                                          
005500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005800     SKIP2                                                                
005900 01  ERRTEXT.                                                             
006000     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
006100     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006201     EJECT                                                                
006202*    --- PARAMETRAR TILL POSTSUM                                          
006203*                                                                         
006210*01  -COPY W0005   -PRE  POSTSUM-                                         
006401     EJECT                                                                
006402 01  IN01-AREA-START             PIC X(24)   VALUE                        
006403                                 'IN01-AREA-START  '.                     
006404     SKIP2                                                                
006405                                                                          
006406*01  AREA -COPY W37164     -PRE IN01-                                     
006407     EJECT                                                                
006408 01  UT01-AREA-START             PIC X(24)   VALUE                        
006409                                 'UT01-AREA-START  '.                     
006410*01  AREA -COPY W37167     -PRE UT01-                                     
006420     SKIP2                                                                
006500     EJECT                                                                
006600 PROCEDURE DIVISION.                                                      
006700 MAIN SECTION.                                                            
006900     SKIP2                                                                
007000                                                                          
007100     PERFORM A-INIT                                                       
007210     PERFORM S01-READ-W37164                                              
007300     PERFORM UNTIL END-OF-W37164                                          
007310       IF IN01-KDEXCHA   >  0                                             
007400         IF IN01-KDBEH NOT = 'D'                                          
007510           PERFORM B-MAKE-UTPOST                                          
007600           PERFORM S11-WRITE-W37167                                       
007900         END-IF                                                           
008000       END-IF                                                             
008010       PERFORM S01-READ-W37164                                            
008100     END-PERFORM                                                          
008200                                                                          
008300                                                                          
008400     PERFORM Z-FINIT                                                      
008500                                                                          
008600     MOVE ZERO TO RETURN-CODE                                             
008700     GOBACK                                                               
008800     .                                                                    
008900     EJECT                                                                
009000 A-INIT SECTION.                                                          
009101                                                                          
009110     OPEN INPUT  W37164                                                   
009201                                                                          
009210     OPEN OUTPUT W37167                                                   
009300     SKIP2                                                                
009400     ACCEPT TODAYS-DATE  FROM DATE                                        
009510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009520     MOVE 1 TO COUNTER                                                    
009600     .                                                                    
009700     EJECT                                                                
009710 B-MAKE-UTPOST SECTION.                                                   
009711                                                                          
009712     MOVE      0          TO UT01-SOR0-IDDISTR                            
009713     MOVE      0          TO UT01-SOR0-IDKUNDNR                           
009714     MOVE      0          TO UT01-SOR0-IDRONR                             
009715     MOVE      0          TO UT01-SOR0-TIRODAT                            
009716     MOVE    'RKO'        TO UT01-SOR0-IDPTYP                             
009717     MOVE   COUNTER       TO UT01-SOR0-IDLOPNR                            
009718     MOVE    'RKO'        TO UT01-IDPTYP                                  
009719     MOVE IN01-IDARTNR    TO UT01-IDARTNR                                 
009720     MOVE IN01-KVPOINT    TO UT01-KVPOINT                                 
009721     MOVE IN01-KDEXCHA    TO UT01-KDEXCHA                                 
009722     .                                                                    
009730     EJECT                                                                
009740                                                                          
009800 Z-FINIT SECTION.                                                         
009901     CLOSE W37164                                                         
009910           W37167                                                         
010001     SKIP2                                                                
010002     MOVE 'S' TO POSTSUM-OPKOD                                            
010010     CALL POSTSUM USING POSTSUM-PARM                                      
010100     .                                                                    
010201     EJECT                                                                
010202 S01-READ-W37164  SECTION.                                                
010203     READ W37164 INTO IN01-AREA                                           
010204     AT END                                                               
010205        MOVE HIGH-VALUE TO IN01-AREA                                      
010206        SET END-OF-W37164 TO TRUE                                         
010207                                                                          
010208     NOT AT END                                                           
010209        MOVE 'W37164' TO POSTSUM-FDNAMN                                   
010210        MOVE 'W37164D1' TO POSTSUM-DDNAMN2                                
010211        MOVE IN01-KDEXCHA TO POSTSUM-TRANSTYP                             
010212        CALL POSTSUM USING POSTSUM-PARM                                   
010213     END-READ                                                             
010220     .                                                                    
010301     EJECT                                                                
010302 S11-WRITE-W37167 SECTION.                                                
010303                                                                          
010304     WRITE UT-POST   FROM UT01-AREA                                       
010305     ADD 1 TO COUNTER GIVING COUNTER                                      
010306                                                                          
010307     MOVE UT01-IDPTYP TO POSTSUM-TRANSTYP                                 
010308     MOVE 'W37167' TO POSTSUM-FDNAMN                                      
010309     MOVE 'W37164D2' TO POSTSUM-DDNAMN2                                   
010310     CALL POSTSUM USING POSTSUM-PARM                                      
010320     .                                                                    
010500     EJECT                                                                
010600 S99-ABEND SECTION.                                                       
010700                                                                          
010801     SKIP2                                                                
010802     MOVE 'S' TO POSTSUM-OPKOD                                            
010810     CALL POSTSUM USING POSTSUM-PARM                                      
010900     CALL ABEND USING RKOD-ABEND                                          
011000     .                                                                    
