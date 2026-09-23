010000 ID DIVISION.                                                             
020008 PROGRAM-ID.     W0050100.                                                
030008 AUTHOR.         RICHARD.                                                 
050008 DATE-WRITTEN.   APRIL 75.                                                
051007 DATE-COMPILED.                                                           
060004                                                                          
070008*    FUNKTION.   DUMMY-PROGRAM FÖR FORMAT W0F501                          
080008*                WSLOGON OCH WELOGON.                                     
081008*                (LOGON TILL IMS).                                        
090000     SKIP3                                                                
100000 ENVIRONMENT DIVISION.                                                    
110000     SKIP3                                                                
120000 DATA DIVISION.                                                           
130000     EJECT                                                                
140000 WORKING-STORAGE SECTION.                                                 
140113                                                                          
140213*    -- CHECKED BY WY2000                                                 
141009 77    IDPGM                 PIC X(8)    VALUE 'W0050100'.                
142009 77    W-COMPILED            PIC X(16)   VALUE SPACE.                     
143011 77    FELTEXT               PIC X(80)   VALUE SPACE.                     
150009 77    JA                    PIC X       VALUE 'J'.                       
160009 77    NEJ                   PIC X       VALUE 'N'.                       
161008                                                                          
170009 01    W-IDTRANS             PIC X(4)    VALUE SPACE.                     
170010       88  SPLITTAD-IDTRANS              VALUE '4213' '4223'              
170011                                               '4233' '4243'              
170012                                               '0813'.                    
170013                                                                          
170020 01    DYNAMISKA-SUBPROGRAM.                                              
180009   03    CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
190009   03    FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
200000     EJECT                                                                
210000******************************************************************        
220000*                                                                         
230000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
240000*                                                                         
250009 01    FILLER                PIC X(16)   VALUE 'MFS-WS          '.        
260000     SKIP3                                                                
270009*01    -COPY WMSGAREA                                                     
290000   03    MSG-MOD             REDEFINES MSG-AREA.                          
300009     05    MOD-IDTRANS       PIC X(4).                                    
301009     05    MOD-KDMFSFOR      PIC X(1).                                    
310009     05    MOD-TEMFSFEL      PIC X(40).                                   
310010   03    MSG-MOD-SPLIT       REDEFINES MSG-AREA.                          
310011     05    FILLER             PIC X(2).                                   
310020     05    MOD-IDTRANS1-SPLIT PIC X(1).                                   
310021     05    FILLER             PIC X(2).                                   
310022     05    MOD-IDTRANS2-SPLIT PIC X(1).                                   
310023     05    FILLER             PIC X(2).                                   
310024     05    MOD-IDTRANS3-SPLIT PIC X(1).                                   
310025     05    FILLER             PIC X(2).                                   
310026     05    MOD-IDTRANS4-SPLIT PIC X(1).                                   
310030     05    MOD-KDMFSFOR-SPLIT PIC X(1).                                   
310040     05    MOD-TEMFSFEL-SPLIT PIC X(40).                                  
320000     EJECT                                                                
330009*01    -COPY WMSGSPAR                                                     
350000     EJECT                                                                
360000******************************************************************        
370000*                                                                         
380000*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
390000*                                                                         
400009 01    IMS-WS.                                                            
410009   03    FILLER              PIC X(16)   VALUE 'MS-WS '.                  
420000     SKIP3                                                                
430009   03    STATUS-WS           PIC XX.                                      
440009     88    SEGMENT-FINNS                 VALUE '  '.                      
450009     88    SEGMENT-SAKNAS                VALUE 'GE'.                      
460000     SKIP3                                                                
470000   03    GODK-STATUSKODER.                                                
480000     05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.                
490012     EJECT                                                                
500009*01    -COPY W0003                                                        
520000     EJECT                                                                
530000 LINKAGE SECTION.                                                         
540009*01    -COPY W0009     -PRE MSG-                                          
560000     EJECT                                                                
570000 PROCEDURE DIVISION USING MSG-PCB.                                        
580008 MAIN SECTION.                                                            
581008     ENTRY 'DLITCBL' USING MSG-PCB.                                       
590000                                                                          
600000     PERFORM IMS-GET-MSG                                                  
610000     IF SEGMENT-FINNS                                                     
620009       PERFORM A-INIT-SPARA-INPUT                                         
620010       IF SPLITTAD-IDTRANS                                                
620011          MOVE MSG-SPAR-IDTRANS(1:1) TO MOD-IDTRANS1-SPLIT                
620012          MOVE MSG-SPAR-IDTRANS(2:1) TO MOD-IDTRANS2-SPLIT                
620013          MOVE MSG-SPAR-IDTRANS(3:1) TO MOD-IDTRANS3-SPLIT                
620014          MOVE MSG-SPAR-IDTRANS(4:1) TO MOD-IDTRANS4-SPLIT                
620015          MOVE SPACE                 TO MOD-KDMFSFOR-SPLIT                
620016          IF SWEDISH-TEXT                                                 
620017            MOVE 'FEL BILD VALD'     TO MOD-TEMFSFEL-SPLIT                
620018          ELSE                                                            
620019            MOVE 'WRONG PICTURE SELECTED' TO MOD-TEMFSFEL                 
620020          END-IF                                                          
620021          MOVE +56 TO MSG-KVLL                                            
620022       ELSE                                                               
620023          MOVE MSG-SPAR-IDTRANS      TO MOD-IDTRANS                       
620024          MOVE SPACE                 TO MOD-KDMFSFOR                      
620025          IF SWEDISH-TEXT                                                 
620026            MOVE 'FEL BILD VALD'     TO MOD-TEMFSFEL                      
620027          ELSE                                                            
620028            MOVE 'WRONG PICTURE SELECTED' TO MOD-TEMFSFEL                 
620029          END-IF                                                          
620030          MOVE +48 TO MSG-KVLL                                            
620031       END-IF                                                             
620040       MOVE MSG-MOD-NAME             TO MSG-SPAR-MODNAMN                  
620050       INSPECT MSG-SPAR-MODNAMN REPLACING LEADING 'I' BY 'O'              
720000       PERFORM IMS-INSERT-MSG                                             
730000     END-IF                                                               
740000     MOVE ZERO TO RETURN-CODE                                             
750000     GOBACK                                                               
760000     .                                                                    
770000     EJECT                                                                
780009 A-INIT-SPARA-INPUT SECTION.                                              
790009                                                                          
791009     MOVE WHEN-COMPILED TO W-COMPILED                                     
791109                                                                          
792008     IF MSG-DUBBLA-TRANSKODER                                             
810020        MOVE MSG-IDTRANS-2  TO MSG-SPAR-IDTRANS                           
810030        MOVE MSG-KDMFSFOR-2 TO MSG-SPAR-KDMFSFOR                          
820000     ELSE                                                                 
840020        MOVE MSG-IDTRANS-1  TO MSG-SPAR-IDTRANS                           
840030        MOVE MSG-KDMFSFOR-1 TO MSG-SPAR-KDMFSFOR                          
850000     END-IF                                                               
850100     MOVE MSG-SPAR-IDTRANS  TO W-IDTRANS                                  
860000     .                                                                    
870000     EJECT                                                                
880000* IMS SEKTIONER                                                           
890000     SKIP3                                                                
900000 IMS-GET-MSG SECTION.                                                     
910000     MOVE '  QC' TO GODK-STATUSKODER                                      
920000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
930000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
940000     PERFORM IMS-STATUSKONTROLL                                           
950000     .                                                                    
960000     SKIP3                                                                
970000 IMS-INSERT-MSG SECTION.                                                  
980004     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
990000     MOVE SPACE TO GODK-STATUSKODER                                       
000009     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MSG-SPAR-MODNAMN         
010000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020000     PERFORM IMS-STATUSKONTROLL                                           
030000     .                                                                    
040000     EJECT                                                                
050000 IMS-STATUSKONTROLL SECTION.                                              
060000     SET STATUS-IX TO 1                                                   
070002     SEARCH GODK-STATUS                                                   
071002       AT END                                                             
072011         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT                         
073011         CALL FELLOG                                                      
080002       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
081002         CONTINUE                                                         
090000     END-SEARCH                                                           
100000     .                                                                    
