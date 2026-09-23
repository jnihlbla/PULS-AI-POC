000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4130200.                                                
000400 AUTHOR.         LUC FEYS.                                                
000500 DATE-WRITTEN.   90/05/28.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*                                                                         
001100*        THE PROGRAM IS A SB                                              
001200*        PROGRAM READS      WLORQA (WDQ3)                                 
001300*        CREATES A SEQUENTIAL ORDERPARTFILE                               
001400*                                                                         
001500*    INDATA.                                                              
001600*        DB           WDQ3                                                
001700*                                                                         
001800*   OUTDATA.                                                              
001900*        FILE         W41302                                              
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*    ORDERPARTSFILE W41302   OUTPUT                                       
002700     SELECT W41302 ASSIGN TO W41302D1.                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100      SKIP2                                                               
003200 FD  W41302                                                               
003300     LABEL RECORD STANDARD                                                
003400     RECORDING F                                                          
003500     BLOCK CONTAINS 0.                                                    
003600 01  W41302-OUT  -COPY WDQ301 -L                                          
003700     SKIP2                                                                
003800 WORKING-STORAGE SECTION.                                                 
003801                                                                          
003810*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W4130200'.            
004000                                                                          
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300                                                                          
004400                                                                          
004500                                                                          
004600     EJECT                                                                
004700*    --- SUBPROGRAM AND PARAMETER-AREAS                                   
004800 01  GENERAL-SUBPROGRAM.                                                  
004900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI'.             
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200     EJECT                                                                
005300*    --- PARAMETERS FOR  SUBPROGRAM POSTSUM                               
005400*   -COPY W0005 -PRE POSTSUM-.                                            
005600     SKIP3                                                                
005700     EJECT                                                                
005800*    --- WORK-AREAS FOR IMS-SECTIONS                                      
005900*                                                                         
006000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006100     SKIP3                                                                
006200 01  KEYS-TILL-DLI.                                                       
006300     03  W-WDQ301KY-X.                                                    
006400         05  W-WDQ301KY          PIC X(11)    VALUE SPACE.                
006500*    --- STATUS-CODE FROM IMS                                             
006600 01  STATUS-WS                   PIC XX.                                  
006700     88  SEGMENT-FOUND                       VALUE '  '.                  
006800     88  SEGMENT-END                         VALUE 'GB'.                  
006900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
007000     SKIP2                                                                
007100 01  GOOD-STATUSCODES.                                                    
007200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007300     SKIP3                                                                
007400     EJECT                                                                
007500*    --- IMS FUNCTIONCODES                                                
007600*01  -COPY W0003                                                          
007800     EJECT                                                                
007900*    ---  DLI INPUT-OUTPUT AREA                                           
008000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
008100     SKIP3                                                                
008200 01  DLI-IO-AREA.                                                         
008300     03  IO-AREA                 PIC X(192)  VALUE SPACE.                 
008400     SKIP3                                                                
008500     03  WLORQA01 REDEFINES IO-AREA.                                      
008600*        05  -COPY WDQ301     -PRE ORQA-                                  
008800     EJECT                                                                
008900 LINKAGE SECTION.                                                         
009000                                                                          
009100*01  -COPY W0008      -PRE ORQA-                                          
009300     05  FILLER                  PIC X.                                   
009400     EJECT                                                                
009500 PROCEDURE DIVISION  USING  ORQA-PCB.                                     
009600     ENTRY 'DLITCBL' USING  ORQA-PCB.                                     
009700                                                                          
009800     PERFORM A-INIT                                                       
009900     PERFORM IMS-GET-ORQA                                                 
010000     PERFORM UNTIL SEGMENT-END                                            
010100        PERFORM B-WRITE-OUTFIL                                            
010200        PERFORM IMS-GET-ORQA                                              
010300     END-PERFORM                                                          
010400     PERFORM Z-FINAL                                                      
010500     MOVE ZERO TO RETURN-CODE                                             
010600     GOBACK                                                               
010700     .                                                                    
010800     EJECT                                                                
010900 A-INIT SECTION.                                                          
011000                                                                          
011100     OPEN OUTPUT W41302                                                   
011200     MOVE 'W41302'             TO POSTSUM-PROGNAMN                        
011300     MOVE 'W41302D1'           TO POSTSUM-DDNAMN2                         
011400     MOVE 'W41302  '           TO POSTSUM-FDNAMN                          
011500     .                                                                    
011600     EJECT                                                                
011700 B-WRITE-OUTFIL SECTION.                                                  
011800                                                                          
011900     WRITE W41302-OUT FROM ORQA-ODEL-WDQ301                               
012000                                                                          
012100     CALL POSTSUM    USING POSTSUM-PARM                                   
012200     .                                                                    
012300     EJECT                                                                
012400 Z-FINAL  SECTION.                                                        
012500                                                                          
012600     CLOSE W41302                                                         
012700                                                                          
012800     MOVE 'S' TO POSTSUM-OPKOD                                            
012900       CALL POSTSUM  USING POSTSUM-PARM                                   
013000     .                                                                    
013100     EJECT                                                                
013200 IMS-GET-ORQA SECTION.                                                    
013300                                                                          
013400     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
013500     CALL CBLTDLI USING GN ORQA-PCB DLI-IO-AREA                           
013600     MOVE ORQA-STATUS-CODE TO STATUS-WS                                   
013700     PERFORM IMS-STATUSCONTROL                                            
013800     .                                                                    
013900     EJECT                                                                
014000 IMS-STATUSCONTROL SECTION.                                               
014100                                                                          
014200     SET STATUS-IX TO 1                                                   
014300     SEARCH GOOD-STATUS                                                   
014400       AT END CALL FELLOG                                                 
014500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
014600     END-SEARCH                                                           
014700     .                                                                    
