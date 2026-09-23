000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W0050200.                                                
000400 AUTHOR.         RICHARD.                                                 
000500 DATE-WRITTEN.   APRIL 75.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION.   DUMMY-PROGRAM FÖR FORMAT W0F502                          
000900*                WSPW OCH WEPW.                                           
001000*                (BYTE AV PASSWORD I IMS).                                
001100     SKIP3                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP3                                                                
001400 DATA DIVISION.                                                           
001500     EJECT                                                                
001600 WORKING-STORAGE SECTION.                                                 
001601                                                                          
001610*    -- CHECKED BY WY2000                                                 
001700 77    IDPGM                 PIC X(8)    VALUE 'W0050200'.                
001800 77    W-COMPILED            PIC X(16)   VALUE SPACE.                     
001810 77    FELTEXT               PIC X(80)   VALUE SPACE.                     
001900 77    JA                    PIC X       VALUE 'J'.                       
002000 77    NEJ                   PIC X       VALUE 'N'.                       
002100                                                                          
002200 01    DYNAMISKA-SUBPROGRAM.                                              
002300   03    CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
002400   03    FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
002500     EJECT                                                                
002600******************************************************************        
002700*                                                                         
002800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
002900*                                                                         
003000 01    FILLER                PIC X(16)   VALUE 'MFS-WS          '.        
003100     SKIP3                                                                
003200*01    -COPY WMSGAREA                                                     
003300   03    MSG-MOD             REDEFINES MSG-AREA.                          
003400     05    MOD-IDTRANS       PIC X(4).                                    
003500     05    MOD-KDMFSFOR      PIC X(1).                                    
003600     05    MOD-TEMFSFEL      PIC X(40).                                   
003700     EJECT                                                                
003800*01    -COPY WMSGSPAR                                                     
003900     EJECT                                                                
004000******************************************************************        
004100*                                                                         
004200*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
004300*                                                                         
004400 01    IMS-WS.                                                            
004500   03    FILLER              PIC X(16)   VALUE 'MS-WS '.                  
004600     SKIP3                                                                
004700   03    STATUS-WS           PIC XX.                                      
004800     88    SEGMENT-FINNS                 VALUE '  '.                      
004900     88    SEGMENT-SAKNAS                VALUE 'GE'.                      
005000     SKIP3                                                                
005100   03    GODK-STATUSKODER.                                                
005200     05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.                
005300     EJECT                                                                
005400*01    -COPY W0003                                                        
005500     EJECT                                                                
005600 LINKAGE SECTION.                                                         
005700*01    -COPY W0009     -PRE MSG-                                          
005800     EJECT                                                                
005900 PROCEDURE DIVISION USING MSG-PCB.                                        
006000 MAIN SECTION.                                                            
006100     ENTRY 'DLITCBL' USING MSG-PCB.                                       
006200                                                                          
006300     PERFORM IMS-GET-MSG                                                  
006400     IF SEGMENT-FINNS                                                     
006500       PERFORM A-INIT-SPARA-INPUT                                         
006600       MOVE MSG-SPAR-IDTRANS TO MOD-IDTRANS                               
006700       MOVE SPACE TO MOD-KDMFSFOR                                         
006800       MOVE MSG-MOD-NAME TO MSG-SPAR-MODNAMN                              
006900       INSPECT MSG-SPAR-MODNAMN REPLACING LEADING 'I' BY 'O'              
007000       IF SWEDISH-TEXT                                                    
007100         MOVE 'FEL BILD VALD' TO MOD-TEMFSFEL                             
007200       ELSE                                                               
007300         MOVE 'WRONG PICTURE SELECTED' TO MOD-TEMFSFEL                    
007400       END-IF                                                             
007500       MOVE +48 TO MSG-KVLL                                               
007600       PERFORM IMS-INSERT-MSG                                             
007700     END-IF                                                               
007800     MOVE ZERO TO RETURN-CODE                                             
007900     GOBACK                                                               
008000     .                                                                    
008100     EJECT                                                                
008200 A-INIT-SPARA-INPUT SECTION.                                              
008300                                                                          
008400     MOVE WHEN-COMPILED TO W-COMPILED                                     
008500                                                                          
008600     IF MSG-DUBBLA-TRANSKODER                                             
008700       MOVE MSG-IDTRANS-2 TO MSG-SPAR-IDTRANS                             
008800       MOVE MSG-KDMFSFOR-2 TO MSG-SPAR-KDMFSFOR                           
008900     ELSE                                                                 
009000       MOVE MSG-IDTRANS-1 TO MSG-SPAR-IDTRANS                             
009100       MOVE MSG-KDMFSFOR-1 TO MSG-SPAR-KDMFSFOR                           
009200     END-IF                                                               
009300     .                                                                    
009400     EJECT                                                                
009500* IMS SEKTIONER                                                           
009600     SKIP3                                                                
009700 IMS-GET-MSG SECTION.                                                     
009800     MOVE '  QC' TO GODK-STATUSKODER                                      
009900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
010000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
010100     PERFORM IMS-STATUSKONTROLL                                           
010200     .                                                                    
010300     SKIP3                                                                
010400 IMS-INSERT-MSG SECTION.                                                  
010500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
010600     MOVE SPACE TO GODK-STATUSKODER                                       
010700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MSG-SPAR-MODNAMN         
010800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
010900     PERFORM IMS-STATUSKONTROLL                                           
011000     .                                                                    
011100     EJECT                                                                
011200 IMS-STATUSKONTROLL SECTION.                                              
011300     SET STATUS-IX TO 1                                                   
011400     SEARCH GODK-STATUS                                                   
011500       AT END                                                             
011600         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT                         
011610         CALL FELLOG                                                      
011700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
011800         CONTINUE                                                         
011900     END-SEARCH                                                           
012000     .                                                                    
