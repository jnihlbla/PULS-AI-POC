000100 ID DIVISION.                                                             
000200 PROGRAM-ID.       W9104400.                                              
000300 AUTHOR.           INGRID DANIELSSON.                                     
000400 DATE-COMPILED.                                                           
000500 DATE-WRITTEN.     MARS 1982.                                             
000600     REMARKS.                                                             
000700*                     SB-PROGRAM                                          
000800*        PROGRAMMET EXTRAHERAR EN FILE MED LEVERANTÖRS-UPPGIFTER          
000900*        FRÅN CROSS-INDEX REGISTRET.                                      
001000*                                                                         
001100*        ÄNDRAT  => COBOL-II  880413 AV INGRID DANIELSSON                 
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600         SELECT CROSS-INDEX-UT ASSIGN TO UT-S-W91044D1.                   
001700     EJECT                                                                
001800 DATA DIVISION.                                                           
001900 FILE SECTION.                                                            
002000 FD      CROSS-INDEX-UT                                                   
002100         LABEL RECORD STANDARD                                            
002200         RECORDING MODE F                                                 
002300         BLOCK CONTAINS 0 RECORDS.                                        
002400     SKIP2                                                                
002500*01  POST    -COPY W91044L1C0    -PRE UT44-    -L.                        
002600*++INCLUDE W91044L1C0                                                     
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    -- CHECKED BY WY2000                                                 
300000 77      SKRIV-SW        PIC X       VALUE 'N'.                           
310000 77      JA              PIC X       VALUE 'J'.                           
320000 77      NEJ             PIC X       VALUE 'N'.                           
330000     SKIP3                                                                
331000 01  WS-IDARTNR          PIC S9(9)   COMP-3.                              
332000 01  WS-IDARTNR-01       PIC S9(9)   COMP-3.                              
340000 01  DYNAMISKA-SUBPROGRAM.                                                
350000     03  CBLTDLI         PIC X(8)    VALUE 'CBLTDLI'.                     
360000     EJECT                                                                
370000*01  AREA    -COPY W91044L1C0    -PRE L1-                                 
380000*++INCLUDE W91044L1C0                                                     
390000     EJECT                                                                
400000*- - - - - ARBETSAREOR FÖR IMS-SEKTIONERNA                                
410000 01  FILLER         PIC X(16)     VALUE 'IMS-WS  '.                       
420000     SKIP3                                                                
430000 01  STATUS-WS      PIC X(2).                                             
440000     88  SEGMENT-FINNS         VALUE '  '.                                
450000     88  SEGMENT-SAKNAS        VALUE 'GE'.                                
460000     88  SEGMENT-SLUT          VALUE 'GB'.                                
470000     SKIP3                                                                
480000 01  GODK-STATUSKODER.                                                    
490000     05  GODK-STATUS  PIC X(2)   OCCURS 5  INDEXED BY STATUS-IX.          
500000     SKIP3                                                                
510000 01  SSA1             PIC X(32).                                          
520000     EJECT                                                                
530000*    -COPY W0003                                                          
540000*++INCLUDE W0003CCCC0                                                     
550000     EJECT                                                                
560000 01  FILLER              PIC X(16)  VALUE 'DLI-IO-AREA'.                  
570000     SKIP3                                                                
580000 01      DLI-IO-AREA     PIC X(200).                                      
590000     SKIP3                                                                
600000*01  WDF501  -COPY WDF501  -RED DLI-IO-AREA.                              
610000 ++INCLUDE WDF501CCC0                                                     
620000     EJECT                                                                
630000*01  WDF502  -COPY WDF502  -RED DLI-IO-AREA.                              
640000 ++INCLUDE WDF502CCC0                                                     
650000     EJECT                                                                
660000 LINKAGE SECTION.                                                         
670000     SKIP3                                                                
680000*01  -COPY W0008     -PRE WDF5-                                           
690000*++INCLUDE W0008CCCC0                                                     
700000     05  FILLER        PIC X.                                             
710000     EJECT                                                                
720000 PROCEDURE DIVISION USING WDF5-PCB.                                       
730000     ENTRY 'DLITCBL' USING WDF5-PCB.                                      
740000                                                                          
750000     OPEN OUTPUT CROSS-INDEX-UT                                           
760000     PERFORM IMS-GN-SEGMENT                                               
770000                                                                          
780000     PERFORM UNTIL  SEGMENT-SLUT                                          
790000       EVALUATE WDF5-SEG-NAME-FB                                          
800000                                                                          
810000         WHEN 'WDF501'    MOVE XART-IDARTNR    TO L1-IDARTNR              
811000                                                WS-IDARTNR-01             
820000                          MOVE XART-REKSIFFR   TO L1-REKSIFFR             
830000                          MOVE XART-KDFTAG     TO L1-KDFTAG               
831000*                         MOVE SPACE         TO L1-IDLEVNR                
840000                                                                          
850000         WHEN 'WDF502'    IF SKRIV-SW = JA                                
860000                            IF  XLEV-IDLEVNR = L1-IDLEVNR                 
861000                            AND WS-IDARTNR-01 = WS-IDARTNR                
862000                              CONTINUE                                    
863000                            ELSE                                          
870000                              WRITE UT44-POST                             
880000                              MOVE NEJ TO SKRIV-SW                        
890000                            END-IF                                        
900000                          END-IF                                          
910000                          MOVE XLEV-IDLEVNR   TO L1-IDLEVNR               
920000                          MOVE XLEV-FLTLVM    TO L1-FLTLVM                
930000                          MOVE XLEV-BELEVART  TO L1-BELEV                 
940000                                                                          
950000                          MOVE L1-AREA      TO UT44-POST                  
960000                          MOVE JA TO SKRIV-SW                             
961000                          MOVE WS-IDARTNR-01   TO WS-IDARTNR              
970000         END-EVALUATE                                                     
980000         PERFORM IMS-GN-SEGMENT                                           
990000     END-PERFORM                                                          
000000                                                                          
010000     IF SKRIV-SW = JA                                                     
020000       WRITE UT44-POST                                                    
030000     END-IF                                                               
040000     CLOSE CROSS-INDEX-UT                                                 
050000                                                                          
060000     MOVE ZERO TO RETURN-CODE                                             
070000     GOBACK                                                               
080000     .                                                                    
090000     EJECT                                                                
100000 IMS-GN-SEGMENT       SECTION.                                            
110000                                                                          
120000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
130000     CALL CBLTDLI USING GN WDF5-PCB DLI-IO-AREA                           
140000     MOVE WDF5-STATUS-CODE TO STATUS-WS                                   
150000     PERFORM IMS-STATUSKONTROLL                                           
160000     .                                                                    
170000     SKIP3                                                                
180000 IMS-STATUSKONTROLL   SECTION.                                            
190000                                                                          
200000     SET STATUS-IX TO 1                                                   
210000     SEARCH GODK-STATUS                                                   
220000            WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                      
230000                 CONTINUE                                                 
240000     END-SEARCH                                                           
250000     .                                                                    
