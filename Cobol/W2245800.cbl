000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2245800.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/03/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        INPUT FILE                                                       
001000*          PARAMETER FILE FROM SCREEN 2111                                
001100*          NDC:KINA LAGERBAND SORTERAT ARTIKELNR, DC (71, 72, 73)         
001110*          NDC:USA  LAGERBAND SORTERAT ARTIKELNR, DC (4*)                 
001120*          NDC'R MED LOKAL ANSKAFFNING                                    
001200*                                                                         
001300*        OUTPUT FILE                                                      
001400*          PARTS TO LOGG ON WDGX2213/2214                                 
001500*                                                                         
001600*    ABENDCODES:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- PARAMETER FILE FROM SCREEN 2111                            
002900     SELECT W2245801                   ASSIGN TO W22458D1.                
003000     SKIP2                                                                
003100*          --- NDC:ERS  LAGERBAND SORTERAT ARTIKELNR, DC (7X,4X)          
003200     SELECT W01184                     ASSIGN TO W22458D2.                
003300     SKIP2                                                                
003400*          --- FILE TO UPDATE WDGX2213/2214                               
003500     SELECT W2245802                   ASSIGN TO W22458D3.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W2245801                                                             
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500 01  IN-PARM           PIC X(80).                                         
004600     SKIP3                                                                
004700 FD  W01184                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  -COPY W01184      -L.                                                
005200     SKIP3                                                                
005300 FD  W2245802                                                             
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  RECORD -COPY W2245801 -PRE  OUT-  -L.                                
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000                                                                          
006100 77  IDPGM                       PIC X(8)    VALUE 'W2245800'.            
006200 77  YES                         PIC X       VALUE 'J'.                   
006300 77  NOO                         PIC X       VALUE 'N'.                   
006400                                                                          
006500 77  PARM-EOF-SW                 PIC X       VALUE 'N'.                   
006600     88  END-OF-PARM                         VALUE 'J'.                   
006700                                                                          
006800 77  W01184-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W01184                       VALUE 'J'.                   
007000     EJECT                                                                
007100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
007200 01  FILLER REDEFINES TODAYS-DATE.                                        
007300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
007400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007500     03  TODAYS-DATE-DAY         PIC 9(2).                                
007600     EJECT                                                                
007700 01  GENERAL-SUBPROGRAMS.                                                 
007800*                                                                         
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008100     SKIP2                                                                
008200*    --- PARAMETERS TO ABEND                                              
008300                                                                          
008400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008700     SKIP2                                                                
008800 01  ERROR-TEXT.                                                          
008900     03  FILLER                  PIC X(08)   VALUE 'ERR-TEXT'.            
009000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009100     EJECT                                                                
009200*    --- PARAMETRAR TILL POSTSUM                                          
009300*                                                                         
009400*01  -COPY W0005   -PRE  POSTSUM-                                         
009500     EJECT                                                                
009600 01  IN-PARM-START               PIC X(24)   VALUE                        
009700                                 'IN-PARM-START  '.                       
009800 01  PARM-AREA.                                                           
009900     05  IN-PARM-IDLEVNR         PIC X(05) VALUE SPACE.                   
010000     05  FILLER                  PIC X(01) VALUE SPACE.                   
010100     05  IN-PARM-IDDC            PIC X(02) VALUE SPACE.                   
010200     05  FILLER                  PIC X(72) VALUE SPACE.                   
010300     EJECT                                                                
010400 01  LB-AREA-START               PIC X(24)   VALUE                        
010500                                 'LB-AREA-START  '.                       
010600     SKIP2                                                                
010700                                                                          
010800*01  SLAG-AREA -COPY W01184                                               
010900     EJECT                                                                
011000 01  OUT-AREA-START              PIC X(24)   VALUE                        
011100                                 'OUT-AREA-START  '.                      
011200     SKIP2                                                                
011300                                                                          
011400*01  AREA -COPY W2245801     -PRE OUT-                                    
011500     EJECT                                                                
011600 PROCEDURE DIVISION.                                                      
011700 MAIN SECTION.                                                            
011800                                                                          
011900     PERFORM A-INIT                                                       
012000                                                                          
012100     PERFORM S01-READ-W2245801                                            
012200                                                                          
012300     PERFORM S02-READ-W01184                                              
012400     PERFORM UNTIL END-OF-W01184                                          
012500                                                                          
012600       IF SLAG-IDDC         = IN-PARM-IDDC                                
012700      AND SLAG-IDLEVNR-SHIP = IN-PARM-IDLEVNR                             
012800      AND SLAG-IDDC-REF     = SPACE                                       
012900          MOVE SLAG-IDARTNR TO OUT-IDARTNR                                
013000          MOVE SLAG-IDDC    TO OUT-IDDC                                   
013100          PERFORM S11-WRITE-W2245802                                      
013200       END-IF                                                             
013300                                                                          
013400       PERFORM S02-READ-W01184                                            
013500                                                                          
013600     END-PERFORM                                                          
013700                                                                          
013800     PERFORM Z-FINIT                                                      
013900                                                                          
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500                                                                          
014600     OPEN INPUT  W2245801                                                 
014700                 W01184                                                   
014800                                                                          
014900     OPEN OUTPUT W2245802                                                 
015000     SKIP2                                                                
015100     ACCEPT TODAYS-DATE  FROM DATE                                        
015200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015300     .                                                                    
015400     EJECT                                                                
015500 Z-FINIT SECTION.                                                         
015600     CLOSE W2245801                                                       
015700           W01184                                                         
015800           W2245802                                                       
015900     SKIP2                                                                
016000     MOVE 'S' TO POSTSUM-OPKOD                                            
016100     CALL POSTSUM USING POSTSUM-PARM                                      
016200     .                                                                    
016300     EJECT                                                                
016400 S01-READ-W2245801  SECTION.                                              
016500     READ W2245801 INTO PARM-AREA                                         
016600     AT END                                                               
016700        MOVE HIGH-VALUE TO PARM-AREA                                      
016800        SET END-OF-PARM TO TRUE                                           
016900                                                                          
017000     NOT AT END                                                           
017100        MOVE 'W2245801' TO POSTSUM-FDNAMN                                 
017200        MOVE 'W22458D1' TO POSTSUM-DDNAMN2                                
017300*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
017400        MOVE 'PARM'     TO POSTSUM-TRANSTYP                               
017500        CALL POSTSUM USING POSTSUM-PARM                                   
017600     END-READ                                                             
017700     .                                                                    
017800     EJECT                                                                
017900 S02-READ-W01184  SECTION.                                                
018000     READ W01184 INTO SLAG-AREA                                           
018100     AT END                                                               
018200        MOVE HIGH-VALUE   TO SLAG-AREA                                    
018300        SET END-OF-W01184 TO TRUE                                         
018400                                                                          
018500     NOT AT END                                                           
018600        MOVE 'W01184'   TO POSTSUM-FDNAMN                                 
018700        MOVE 'W22458D2' TO POSTSUM-DDNAMN2                                
018800*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
018900        MOVE 'SLAG'     TO POSTSUM-TRANSTYP                               
019000        CALL POSTSUM USING POSTSUM-PARM                                   
019100     END-READ                                                             
019200     .                                                                    
019300     EJECT                                                                
019400 S11-WRITE-W2245802 SECTION.                                              
019500                                                                          
019600     WRITE OUT-RECORD FROM OUT-AREA                                       
019700                                                                          
019800     MOVE 'OUT'      TO POSTSUM-TRANSTYP                                  
019900     MOVE 'W2245802' TO POSTSUM-FDNAMN                                    
020000     MOVE 'W22458D3' TO POSTSUM-DDNAMN2                                   
020100     CALL POSTSUM USING POSTSUM-PARM                                      
020200     .                                                                    
020300     EJECT                                                                
020400 S99-ABEND SECTION.                                                       
020500                                                                          
020600     SKIP2                                                                
020700     MOVE 'S' TO POSTSUM-OPKOD                                            
020800     CALL POSTSUM USING POSTSUM-PARM                                      
020900     CALL ABEND USING RKOD-ABEND                                          
021000     .                                                                    
