000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0050600.                                                
000300 AUTHOR.         RICHARD.                                                 
000400 DATE-WRITTEN.   FEB. 93.                                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*     FUNKTION.   TP-PROGRAM FÖR PARTS MENYHANTERING.                     
000800*                 PROGRAMMET LIGGER 'LOCAL' I V4                          
000900*                 FÖR ACF2 KONTROLL VID P-TO-P-SW.                        
001000                                                                          
001100     SKIP3                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP3                                                                
001400 DATA DIVISION.                                                           
001500     EJECT                                                                
001600 WORKING-STORAGE SECTION.                                                 
001700                                                                          
001800*    -- CHECKED BY WY2000                                                 
180002 77    IDPGM                 PIC X(8)    VALUE 'W0050600'.                
190002 77    W-COMPILED            PIC X(16)   VALUE SPACE.                     
200002 77    FELTEXT               PIC X(80)   VALUE SPACE.                     
210002 77    JA                    PIC X       VALUE 'J'.                       
220002 77    NEJ                   PIC X       VALUE 'N'.                       
230002     SKIP3                                                                
240002 01    DYNAMISKA-SUBPROGRAM.                                              
250002   03    CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
260002   03    FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
270002     EJECT                                                                
280002******************************************************************        
290002*                                                                         
300002*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
310002*                                                                         
320002 01    FILLER                PIC X(16)   VALUE 'MFS-WS'.                  
330002     SKIP3                                                                
340002 01    MID-W0I50601.                                                      
350002   03    MID-DATA.                                                        
360002     05    MID-KVLL          PIC S9(4)   COMP.                            
370002     05    MID-Z1-Z2         PIC X(2).                                    
380002     05    FILLER            PIC X(13).                                   
390002     05    MID-KEY           PIC X(10).                                   
400002     EJECT                                                                
410002*01    -COPY WMSGSNUF                                                     
420002     EJECT                                                                
430002   03    MOD-MENY                REDEFINES MSG-AREA.                      
440002     05    MOD-IDTRANS       PIC X(4).                                    
450002     05    MOD-KDMFSFOR      PIC X(1).                                    
460002     05    MOD-TEMFSINF      PIC X(55).                                   
550002     EJECT                                                                
560002*01    -COPY WMSGSPAR                                                     
570002     EJECT                                                                
580002******************************************************************        
590002*                                                                         
600002*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
610002*                                                                         
620002 01    IMS-WS.                                                            
630002   03    FILLER              PIC X(16)   VALUE ' IMS-WS     '.            
640002     SKIP3                                                                
650002   03    STATUS-WS           PIC XX.                                      
660002     88    STATUS-OK                     VALUE '  '.                      
670002     88    SEGMENT-FINNS                 VALUE '  '.                      
680002     88    TRANSKOD-FEL                  VALUE 'A1'.                      
690002     88    SECURITY-FEL                  VALUE 'A4'.                      
700002     SKIP3                                                                
710002   03    GODK-STATUSKODER.                                                
720002     05    GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.              
730002     EJECT                                                                
740002*01    -COPY W0003                                                        
750002     EJECT                                                                
760002 LINKAGE SECTION.                                                         
770002*01    -COPY W0009     -PRE MSG-                                          
780002     SKIP2                                                                
790002*01    -COPY W0009     -PRE ALT-                                          
800002     EJECT                                                                
810002 PROCEDURE DIVISION USING MSG-PCB ALT-PCB.                                
820002 MAIN SECTION.                                                            
830002     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB.                               
840002                                                                          
850002     PERFORM IMS-GET-MSG                                                  
860002     IF SEGMENT-FINNS                                                     
030002       PERFORM A-INIT-SPARA-INPUT                                         
040002       PERFORM B-CHANGE                                                   
060002     END-IF                                                               
070002                                                                          
080002     MOVE ZERO TO RETURN-CODE                                             
090002     GOBACK                                                               
100002     .                                                                    
110002     EJECT                                                                
120002 A-INIT-SPARA-INPUT SECTION.                                              
130002                                                                          
140002     MOVE WHEN-COMPILED TO W-COMPILED                                     
150002                                                                          
160002     MOVE MSG-INDATA   TO MID-W0I50601                                    
170002     MOVE MSG-IDTRANS  TO MSG-SPAR-IDTRANS                                
180002     MOVE MSG-KDMFSFOR TO MSG-SPAR-KDMFSFOR                               
190002     MOVE MSG-KDTRTYP  TO MSG-SPAR-KDTRTYP                                
200002     MOVE SPACE        TO MSG-SPAR-IDPFK                                  
210002     .                                                                    
220002     EJECT                                                                
230002 B-CHANGE SECTION.                                                        
240002                                                                          
250002     MOVE LOW-VALUE TO MID-Z1-Z2                                          
260003     IF MID-KVLL > +27                                                    
270003       MOVE +27 TO MID-KVLL                                               
280002     END-IF                                                               
290002     MOVE MID-DATA TO MSG-IO-AREA-SNUF                                    
300002                                                                          
310002     PERFORM IMS-CHANGE-ALTMSG                                            
320002     IF STATUS-OK                                                         
330002       PERFORM IMS-INSERT-ALTMSG                                          
340002     ELSE                                                                 
350002       MOVE MSG-SPAR-IDTRANS  TO MOD-IDTRANS                              
360002       MOVE MSG-SPAR-KDMFSFOR TO MOD-KDMFSFOR                             
370002       IF SECURITY-FEL                                                    
380002         STRING ' SECURITY VIOLATION '                                    
390002                STATUS-WS ' ' MID-DATA                                    
400002                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
410002       ELSE                                                               
420002         STRING ' WRONG PICTURE '                                         
430002                STATUS-WS ' ' MID-DATA                                    
440002                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
450002       END-IF                                                             
460002       MOVE 'W0O50401' TO MSG-SPAR-MODNAMN                                
470002       MOVE +64 TO MSG-KVLL                                               
480002       PERFORM IMS-INSERT-MSG                                             
490002     END-IF                                                               
500002     .                                                                    
510002     EJECT                                                                
520002* IMS SEKTIONER                                                           
530002     SKIP2                                                                
540002 IMS-GET-MSG SECTION.                                                     
550002     MOVE '  QCCF' TO GODK-STATUSKODER                                    
560002     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA-SNUF                       
570002     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
580002     PERFORM IMS-STATUSKONTROLL                                           
590002     .                                                                    
600002     SKIP2                                                                
610002 IMS-INSERT-MSG SECTION.                                                  
620002     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
630002     MOVE SPACE TO GODK-STATUSKODER                                       
640002     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA-SNUF                     
650002                                     MSG-SPAR-MODNAMN                     
660002     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
670002     PERFORM IMS-STATUSKONTROLL                                           
680002     .                                                                    
690002     SKIP2                                                                
700002 IMS-CHANGE-ALTMSG SECTION.                                               
710002     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
720002     MOVE '  A1A4' TO GODK-STATUSKODER                                    
730002     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS                          
740002     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
750002     PERFORM IMS-STATUSKONTROLL                                           
760002     .                                                                    
770002     SKIP2                                                                
780002 IMS-INSERT-ALTMSG SECTION.                                               
790002     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
800002     MOVE SPACE TO GODK-STATUSKODER                                       
810002     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA-SNUF                     
820002     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
830002     PERFORM IMS-STATUSKONTROLL                                           
840002     .                                                                    
850002     EJECT                                                                
860002 IMS-STATUSKONTROLL SECTION.                                              
870002                                                                          
880002     SET STATUS-IX TO 1                                                   
890002     SEARCH GODK-STATUS                                                   
900002       AT END                                                             
910002         MOVE 'FEL STATUSKOD FRÅN IMS ' TO FELTEXT                        
920002         CALL FELLOG                                                      
930002       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
940002         CONTINUE                                                         
950002     END-SEARCH                                                           
960002     .                                                                    
