000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W1169400.                                                
000400 AUTHOR.         BOHLIN HÅKAN.                                            
000500 DATE-WRITTEN.   18/01/10.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        HANDLE INPUT FILE FROM PRICE SC WITH PARTINFO.                   
001100*        FETCH SUGGESTED RETAIL PRICE SO THAT WDC3 DATABASE CAN           
001200*        BE LOADED IN NEXT STEP.                                          
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
002602*          --- PARAMETER FILE (VCOM SENDERTAG)                            
002603     SELECT SYSIN                      ASSIGN TO W11694D1.                
002604     SKIP2                                                                
002605*          --- INPUT FILE WITH RETAIL PRICES PER SALES COMPANY            
002606     SELECT W11694                     ASSIGN TO W11694D2.                
002607     SKIP2                                                                
002608*          --- OUTPUT FILE WITH PRICES WHICH SHOULD UPDATE WDC3           
002610     SELECT W11695                     ASSIGN TO W11694D3.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003201     SKIP3                                                                
003202 FD  SYSIN                                                                
003203     RECORDING       F                                                    
003204     BLOCK CONTAINS  0.                                                   
003205 01  INPARM          PIC X(80).                                           
003206     SKIP3                                                                
003207 FD  W11694                                                               
003208     RECORDING       F                                                    
003209     BLOCK CONTAINS  0.                                                   
003210                                                                          
003211*01  -COPY W11694      -L.                                                
003212     SKIP3                                                                
003213 FD  W11695                                                               
003214     RECORDING       F                                                    
003215     BLOCK CONTAINS  0.                                                   
003216                                                                          
003220*01  RECORD -COPY W11695 -PRE  OUT-  -L.                                  
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W1169400'.            
003700 77  YES                         PIC X       VALUE 'J'.                   
003800 77  NOO                         PIC X       VALUE 'N'.                   
003900 77  W-IDLANDX2-IN               PIC X(2)    VALUE 'IN'.                  
004000 77  W-KDVALISO                  PIC X(3)    VALUE SPACE.                 
004001 77  W-IDARTNR                   PIC 9(9)    VALUE ZERO.                  
004002                                                                          
004003 77  INPARM-EOF-SW               PIC X       VALUE 'N'.                   
004004     88  END-OF-INPARM                       VALUE 'J'.                   
004005                                                                          
004006 77  W11694-EOF-SW               PIC X       VALUE 'N'.                   
004010     88  END-OF-W11694                       VALUE 'J'.                   
004020                                                                          
004030 77  INDATA-SW                   PIC X       VALUE 'N'.                   
004040     88  INDATA-OK                           VALUE 'J'.                   
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
006201     EJECT                                                                
006202*    --- PARAMETRAR TILL POSTSUM                                          
006203*                                                                         
006210*01  -COPY W0005   -PRE  POSTSUM-                                         
006220                                                                          
006401     EJECT                                                                
006402 01  PARM-AREA-START             PIC X(24)   VALUE                        
006403                                 'PARM-AREA-START  '.                     
006404 01  PARM-AREA.                                                           
006405     03  FILLER                  PIC X(6).                                
006406     03  PARM-IDLANDX2           PIC X(2).                                
006407                                                                          
006408     SKIP2                                                                
006410 01  IN-AREA-START               PIC X(24)   VALUE                        
006411                                 'IN-AREA-START    '.                     
006413*01  AREA -COPY W11694     -PRE IN-                                       
006414                                                                          
006415     SKIP2                                                                
006417 01  OUT-AREA-START              PIC X(24)   VALUE                        
006418                                 'OUT-AREA-START  '.                      
006420*01  AREA -COPY W11695     -PRE OUT-                                      
007100     EJECT                                                                
007200 PROCEDURE DIVISION.                                                      
007300 MAIN SECTION.                                                            
007500     SKIP2                                                                
007600                                                                          
007700     PERFORM A-INIT                                                       
007801     PERFORM S01-READ-SYSIN                                               
007803     IF PARM-IDLANDX2 = W-IDLANDX2-IN                                     
007804       MOVE 'INR' TO W-KDVALISO                                           
007805     ELSE                                                                 
007806       DISPLAY 'WRONG IDLANDX2 IN SENDERTAG ' PARM-IDLANDX2               
007807       PERFORM S99-ABEND                                                  
007808     END-IF                                                               
007810     PERFORM S02-READ-W11694                                              
007900     PERFORM UNTIL END-OF-W11694                                          
008000       PERFORM B-CHECK-INPUT                                              
008010       IF INDATA-OK                                                       
008100          PERFORM C-CREATE-OUTPUT                                         
008200       END-IF                                                             
008610       PERFORM S02-READ-W11694                                            
008700     END-PERFORM                                                          
008800                                                                          
008900                                                                          
009000     PERFORM Z-FINIT                                                      
009100                                                                          
009200     MOVE ZERO TO RETURN-CODE                                             
009300     GOBACK                                                               
009400     .                                                                    
009500     EJECT                                                                
009600 A-INIT SECTION.                                                          
009701                                                                          
009702     OPEN INPUT  SYSIN                                                    
009710                 W11694                                                   
009801                                                                          
009810     OPEN OUTPUT W11695                                                   
009900     SKIP2                                                                
010000     ACCEPT TODAYS-DATE  FROM DATE                                        
010110     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
010200     .                                                                    
010210     EJECT                                                                
010220 B-CHECK-INPUT SECTION.                                                   
010230     MOVE YES TO INDATA-SW                                                
010240     IF IN-IDLEVNR = '11' OR '9001'                                       
010241        IF IN-IDARTNR17(1:1) NUMERIC                                      
010242           MOVE ZERO TO TALLY                                             
010243           INSPECT IN-IDARTNR17 TALLYING TALLY                            
010244                           FOR CHARACTERS BEFORE INITIAL SPACE            
010245           IF IN-IDARTNR17(1:TALLY) NUMERIC AND                           
                    TALLY < 10                                                  
010246              MOVE IN-IDARTNR17(1:TALLY) TO W-IDARTNR                     
010247           ELSE                                                           
010248              MOVE NOO TO INDATA-SW                                       
010249           END-IF                                                         
010250        ELSE                                                              
010251           MOVE NOO TO INDATA-SW                                          
010252        END-IF                                                            
010253     ELSE                                                                 
010260        MOVE NOO TO INDATA-SW                                             
010270     END-IF                                                               
           IF INDATA-SW = 'N'                                                   
              DISPLAY IN-IDARTNR17 ' ' IN-IDLEVNR                               
           END-IF                                                               
010292     .                                                                    
010293     EJECT                                                                
010294 C-CREATE-OUTPUT SECTION.                                                 
010295     MOVE W-IDARTNR       TO OUT-IDARTNR                                  
010296     MOVE PARM-IDLANDX2   TO OUT-IDLANDX2                                 
010297     MOVE W-KDVALISO      TO OUT-KDVALISO                                 
010298     MOVE IN-PRARTBTO-SC  TO OUT-PRARTBTO-SC                              
010299     PERFORM S11-WRITE-W11695                                             
010300     .                                                                    
010310     EJECT                                                                
010400 Z-FINIT SECTION.                                                         
010501     CLOSE SYSIN                                                          
010502           W11694                                                         
010510           W11695                                                         
010601     SKIP2                                                                
010602     MOVE 'S' TO POSTSUM-OPKOD                                            
010610     CALL POSTSUM USING POSTSUM-PARM                                      
010700     .                                                                    
010801     EJECT                                                                
010802 S01-READ-SYSIN  SECTION.                                                 
010803     READ SYSIN INTO PARM-AREA                                            
010804     AT END                                                               
010805        MOVE HIGH-VALUE TO PARM-AREA                                      
010806        SET END-OF-INPARM TO TRUE                                         
010807                                                                          
010808     NOT AT END                                                           
010809        MOVE 'SYSIN'    TO POSTSUM-FDNAMN                                 
010810        MOVE 'W11694D1' TO POSTSUM-DDNAMN2                                
010812        MOVE SPACE      TO POSTSUM-TRANSTYP                               
010813        CALL POSTSUM USING POSTSUM-PARM                                   
010814     END-READ                                                             
010815     .                                                                    
010816     EJECT                                                                
010817 S02-READ-W11694  SECTION.                                                
010818     READ W11694 INTO IN-AREA                                             
010819     AT END                                                               
010820        MOVE HIGH-VALUE TO IN-AREA                                        
010821        SET END-OF-W11694 TO TRUE                                         
010822                                                                          
010823     NOT AT END                                                           
010824        MOVE 'W11694'   TO POSTSUM-FDNAMN                                 
010825        MOVE 'W11694D2' TO POSTSUM-DDNAMN2                                
010827        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
010828        CALL POSTSUM USING POSTSUM-PARM                                   
010829     END-READ                                                             
010830     .                                                                    
010901     EJECT                                                                
010902 S11-WRITE-W11695 SECTION.                                                
010903                                                                          
010904     WRITE OUT-RECORD FROM OUT-AREA                                       
010905                                                                          
010906     MOVE SPACE         TO POSTSUM-TRANSTYP                               
010907     MOVE 'W11695'      TO POSTSUM-FDNAMN                                 
010908     MOVE 'W11694D3'    TO POSTSUM-DDNAMN2                                
010909     CALL POSTSUM USING POSTSUM-PARM                                      
010910     .                                                                    
011100     EJECT                                                                
011200 S99-ABEND SECTION.                                                       
011300                                                                          
011401     SKIP2                                                                
011402     MOVE 'S' TO POSTSUM-OPKOD                                            
011410     CALL POSTSUM USING POSTSUM-PARM                                      
011500     CALL ABEND USING RKOD-ABEND                                          
011600     .                                                                    
