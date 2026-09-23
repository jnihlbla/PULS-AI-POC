000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4130100.                                                
000400 AUTHOR.         LUC FEYS.                                                
000500 DATE-WRITTEN.   90/05/28.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*                                                                         
001100*        THE PROGRAM IS A DLI PROGRAM                                     
001200*        IT READS  WDR1  IDHTYP 4447 AND                                  
001300*        CREATES A SEQUENTIAL PRCFILE                                     
001400*                                                                         
001500*    INDATA.                                                              
001600*        DB           WDR101                                              
001700*                                                                         
001800*   OUTDATA.                                                              
001900*        FILE         W41301                                              
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*    PRCFILE W41301   OUTPUT                                              
002700     SELECT W41301 ASSIGN TO W41301D1.                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100      SKIP2                                                               
003200 FD  W41301                                                               
003300     LABEL RECORD STANDARD                                                
003400     RECORDING F                                                          
003500     BLOCK CONTAINS 0.                                                    
003600 01  W41301-OUT  PIC X(152).                                              
003700     SKIP2                                                                
003800 WORKING-STORAGE SECTION.                                                 
003801                                                                          
003810*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W4130100'.            
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004310*      --- VALID IDDC CODES                                               
004320*                                                                         
004330*01    -COPY WWDC99                                                       
004330*01    -COPY WWDCKONS                                                     
004340       EJECT                                                              
004400                                                                          
004500                                                                          
004600     EJECT                                                                
004700*    --- SUBPROGRAM AND PARAMETER-AREAS                                   
004800 01  GENERAL-SUBPROGRAM.                                                  
004900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200     EJECT                                                                
005300 01  PRCFILE.                                                             
005400     03  IDDC                    PIC 9(2).                                
005500     03  PRCTABEL                PIC X(150).                              
005600*    --- PARAMETERS FOR  SUBPROGRAM POSTSUM                               
005700*   -COPY W0005 -PRE POSTSUM-.                                            
005900     SKIP3                                                                
006000     EJECT                                                                
006100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
006200*                                                                         
006300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006400     SKIP3                                                                
006500*    --- STATUS-CODE FROM IMS                                             
006600 01  STATUS-WS                   PIC XX.                                  
006700     88  SEGMENT-FOUND                       VALUE '  '.                  
006800     88  SEGMENT-END                         VALUE 'GB'.                  
006900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
007000     SKIP2                                                                
007100 01  GOOD-STATUSCODES.                                                    
007200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007300 01  SSA1                        PIC X(64).                               
007400 01  KEYS-TILL-DLI.                                                       
007500                                                                          
007600     03 W-XXKH-4447-X.                                                    
007700        05 W-IDHTYP              PIC X(4)    VALUE '4447'.                
007800        05 W-IDDC                PIC 9(2).                                
007900        05 W-4447-LOW-VALUE      PIC X(24)   VALUE LOW-VALUE.             
008000                                                                          
008100     SKIP3                                                                
008200     EJECT                                                                
008300*    --- IMS FUNCTIONCODES                                                
008400*01  -COPY W0003                                                          
008600     EJECT                                                                
008700*    ---  DLI INPUT-OUTPUT AREA                                           
008800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
008900     SKIP3                                                                
009000 01  DLI-IO-AREA.                                                         
009100     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
009200     SKIP3                                                                
009300     03  WLXXKH01 REDEFINES IO-AREA.                                      
009400*        05  -COPY WDGX4447   -PRE XXKH-                                  
009600     03  WLXXKH11 REDEFINES IO-AREA.                                      
009700*        05  -COPY WDGX4448   -PRE XXKH-                                  
009900     EJECT                                                                
010000 LINKAGE SECTION.                                                         
010100                                                                          
010200*01  -COPY W0008      -PRE KH11-                                          
010400     05  FILLER                  PIC X.                                   
010500     EJECT                                                                
010600 PROCEDURE DIVISION  USING  KH11-PCB.                                     
010700     ENTRY 'DLITCBL' USING  KH11-PCB.                                     
010800                                                                          
010900     PERFORM A-INIT                                                       
011000     MOVE WC-CDC-SE TO W-IDDC IDDC WS-IDDC                                
011010                                                                          
011100     PERFORM IMS-GU-KH01                                                  
011200     PERFORM IMS-GNP-KH11                                                 
011210                                                                          
011220     PERFORM UNTIL NDC-AU                                                 
011300       PERFORM UNTIL SEGMENT-MISSING                                      
011400          MOVE XXKH-4448-WDGX4448-CTX TO PRCTABEL                         
011500          PERFORM B-WRITE-OUTFIL                                          
011600          PERFORM IMS-GNP-KH11                                            
011700       END-PERFORM                                                        
011701       ADD  01 TO W-IDDC IDDC                                             
011702       MOVE W-IDDC    TO WS-IDDC                                          
011703       PERFORM IMS-GU-KH01                                                
011704       IF SEGMENT-FOUND                                                   
011705         PERFORM IMS-GNP-KH11                                             
011706       END-IF                                                             
011707     END-PERFORM                                                          
011710                                                                          
012600     PERFORM Z-FINAL                                                      
012700     MOVE ZERO TO RETURN-CODE                                             
012800     GOBACK                                                               
012900     .                                                                    
013000     EJECT                                                                
013100 A-INIT SECTION.                                                          
013200                                                                          
013300     OPEN OUTPUT W41301                                                   
013400     MOVE 'W41301'             TO POSTSUM-PROGNAMN                        
013500     MOVE 'W41301D1'           TO POSTSUM-DDNAMN2                         
013600     MOVE 'W41301  '           TO POSTSUM-FDNAMN                          
013700     .                                                                    
013800     EJECT                                                                
013900 B-WRITE-OUTFIL SECTION.                                                  
014000                                                                          
014100     WRITE W41301-OUT FROM PRCFILE                                        
014200                                                                          
014300     CALL POSTSUM    USING POSTSUM-PARM                                   
014400     .                                                                    
014500     EJECT                                                                
014600 Z-FINAL  SECTION.                                                        
014700                                                                          
014800     CLOSE W41301                                                         
014900                                                                          
015000     MOVE 'S' TO POSTSUM-OPKOD                                            
015100       CALL POSTSUM  USING POSTSUM-PARM                                   
015200     .                                                                    
015300     EJECT                                                                
015400 IMS-GU-KH01 SECTION.                                                     
015500     STRING 'WLXXKH01(WDGXKEY  =' W-XXKH-4447-X ')'                       
015600          DELIMITED BY SIZE INTO SSA1                                     
015700     MOVE '  GE' TO GOOD-STATUSCODES                                      
015800     CALL CBLTDLI USING GU KH11-PCB DLI-IO-AREA SSA1                      
015900     MOVE KH11-STATUS-CODE TO STATUS-WS                                   
016000     PERFORM IMS-STATUSCONTROL                                            
016100     .                                                                    
016200 IMS-GNP-KH11 SECTION.                                                    
016300     MOVE 'WLXXKH11 ' TO SSA1                                             
016400     MOVE '  GE' TO GOOD-STATUSCODES                                      
016500     CALL CBLTDLI USING GNP KH11-PCB DLI-IO-AREA SSA1                     
016600     MOVE KH11-STATUS-CODE TO STATUS-WS                                   
016700     PERFORM IMS-STATUSCONTROL                                            
016800     .                                                                    
016900     EJECT                                                                
017000 IMS-STATUSCONTROL SECTION.                                               
017100                                                                          
017200     SET STATUS-IX TO 1                                                   
017300     SEARCH GOOD-STATUS                                                   
017400       AT END CALL FELLOG                                                 
017500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
017600     END-SEARCH                                                           
017700     .                                                                    
