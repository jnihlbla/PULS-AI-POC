000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W0150100.                                        
000300 AUTHOR.                 RICHARD.                                         
000400 DATE-WRITTEN.           MARS 87.                                         
000410 DATE-COMPILED.                                                           
000500                                                                          
000700*    FUNKTION:   FIX-PROGRAM FÖR ATT TA BORT MEDDELANDE FRÅN              
000800*                IMS INPUT-KÖ.                                            
000900*    INDATA:                                                              
001000*                INFIL MED ANTAL TRANSAR SOM SKALL TAS BORT               
001100*    SUBPROGRAM:                                                          
001200*                FELLOG                                                   
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 CONFIGURATION SECTION.                                                   
001600     SKIP3                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800 FILE-CONTROL.                                                            
001900                                                                          
002000     SELECT INFIL    ASSIGN TO W01501D1.                                  
002100                                                                          
002200 DATA DIVISION.                                                           
002300 FILE SECTION.                                                            
002400                                                                          
002500 FD      INFIL                                                            
002600         LABEL RECORD STANDARD                                            
002700         RECORDING MODE F.                                                
002800 01      IN-POST.                                                         
002900   03    IN-ANTAL        PIC 9(2).                                        
003000   03    FILLER          PIC X(78).                                       
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300     SKIP2                                                                
003301                                                                          
003310*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W0150100'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  EOF-SW                      PIC X       VALUE 'N'.                   
003800 77  W-ANTAL                     PIC S9(3)   VALUE +0    COMP-3.          
003900 77  W-ANTAL-BORT                PIC S9(3)   VALUE +0    COMP-3.          
004000     SKIP3                                                                
004100 01  DYNAMISKA-SUBPROGRAM.                                                
004200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004400     EJECT                                                                
004500**************************************************************            
004600*                                                                         
004700*                      AREOR FÖR MFS OCH SKÄRMHANTERING                   
004800*                                                                         
004900 01    FILLER                    PIC X(8) VALUE 'MFS-WS  '.               
005000     SKIP3                                                                
005100*01  -COPY WMSGSNUF.                                                      
005300     EJECT                                                                
005400*01  -COPY WMSGSPAR.                                                      
005600     EJECT                                                                
005700******************************************************************        
005800*                                                                         
005900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006000*                                                                         
006100 01  IMS-WS.                                                              
006200   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
006300     SKIP3                                                                
006400*                        **** STATUS-KOD FRÅN IMS                         
006500   03  STATUS-WS                 PIC XX.                                  
006600     88  SEGMENT-FINNS                       VALUE '  '.                  
006700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
006800     SKIP3                                                                
006900   03  GODK-STATUSKODER.                                                  
007000     05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.                
007100     SKIP3                                                                
007200 01    SSA1                      PIC X(32).                               
007300     EJECT                                                                
007400*                            IMS FUNKTIONSKODER                           
007500*01    -COPY W0003                                                        
007700     EJECT                                                                
007800 LINKAGE SECTION.                                                         
007900     SKIP3                                                                
008000*01  -COPY W0009   -PRE MSG-.                                             
008200     EJECT                                                                
008300 PROCEDURE DIVISION USING MSG-PCB.                                        
008400 MAIN SECTION.                                                            
008410                                                                          
008500     MOVE +1 TO W-ANTAL                                                   
008600     OPEN INPUT INFIL                                                     
008700     READ INFIL                                                           
008800       AT END MOVE JA TO EOF-SW                                           
008900     END-READ                                                             
009000     IF EOF-SW = NEJ                                                      
009100       IF IN-ANTAL NUMERIC                                                
009200         PERFORM UNTIL W-ANTAL > IN-ANTAL                                 
009300           PERFORM IMS-GET-MSG                                            
009400           IF SEGMENT-FINNS                                               
009500             ADD +1 TO W-ANTAL-BORT                                       
009600             DISPLAY MSG-KVLL ' ' MSG-AREA                                
009700           ELSE                                                           
009800             MOVE IN-ANTAL TO W-ANTAL                                     
009900           END-IF                                                         
010000           ADD +1 TO W-ANTAL                                              
010100         END-PERFORM                                                      
010200         DISPLAY ' ANTAL RENSADE TRANSAR = ' W-ANTAL-BORT                 
010300       ELSE                                                               
010400         DISPLAY 'EJ NUMERISK INPUT '                                     
010500       END-IF                                                             
010600     ELSE                                                                 
010700       DISPLAY 'IN-KORT SAKNAS '                                          
010800     END-IF                                                               
010900     CLOSE INFIL                                                          
011000     MOVE ZERO TO RETURN-CODE                                             
011100     GOBACK                                                               
011200     .                                                                    
011300     EJECT                                                                
011400* IMS SEKTIONER                                                           
011500     SKIP3                                                                
011600 IMS-GET-MSG SECTION.                                                     
011700     MOVE '  QC' TO GODK-STATUSKODER                                      
011800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA-SNUF                       
011900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
012000     PERFORM IMS-STATUSKONTROLL                                           
012100     .                                                                    
012200     SKIP3                                                                
012300 IMS-STATUSKONTROLL SECTION.                                              
012400     SET STATUS-IX TO 1                                                   
012500     SEARCH GODK-STATUS                                                   
012501       AT END                                                             
012510         CALL FELLOG                                                      
012600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
012610         CONTINUE                                                         
012700     END-SEARCH                                                           
012800     .                                                                    
