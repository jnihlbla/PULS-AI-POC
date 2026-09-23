000100*COMPOPT STDSUB=YES                                                       
010000 ID DIVISION.                                                             
020000     SKIP2                                                                
030000 PROGRAM-ID.     W411ARTM.                                                
040000 AUTHOR.         SVANTE BJÖRKBERG.                                        
050000 DATE-WRITTEN.   JULI-90.                                                 
060000                                                                          
070000     REMARKS.                                                             
080000*                                                                         
090000*    DETTA ÄR EN SUBMODUL SOM ANROPAS I ORDER-ENTRY FÖR                   
100000*    ATT LÄGGA UPP EN TOM ROT PÅ ARTIKELREGISTRET WDK9                    
110000*                                                                         
120000*    REGISTER :    WLARTM (WDK9) ARTIKELREGITER                           
130000*                                                                         
140003*    LÄNKAREA :    W411ARTM-CTX                                           
150000                                                                          
160000     SKIP3                                                                
170000 ENVIRONMENT DIVISION.                                                    
180000     EJECT                                                                
190000 DATA DIVISION.                                                           
200000 WORKING-STORAGE SECTION.                                                 
200104                                                                          
201004*    -- CHECKED BY WY2000                                                 
210000 01  IDPGM                       PIC X(08)   VALUE 'W411ARTM'.            
220000 01  JA                          PIC X       VALUE 'J'.                   
230000 01  NEJ                         PIC X       VALUE 'N'.                   
260000     EJECT                                                                
270000*                                                                         
280000 01  GENERELLA-SUBPROGRAM.                                                
290000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
300000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
310000     EJECT                                                                
320000                                                                          
330000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
340000*                                                                         
350000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
360000     SKIP2                                                                
370000*    --- STATUS-KOD FRÅN IMS                                              
380000 01  STATUS-WS                   PIC XX.                                  
390000     88  SEGMENT-FINNS                       VALUE '  '.                  
400000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
410000     SKIP2                                                                
420000 01  GODK-STATUSKODER.                                                    
430000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
440000     SKIP2                                                                
450000 01  SSA1                        PIC X(64).                               
460000 01  SSA2                        PIC X(64).                               
470000     EJECT                                                                
480000                                                                          
490000*    --- IMS FUNKTIONSKODER                                               
500002*01  -COPY W0003                                                          
520000     EJECT                                                                
530000                                                                          
540000 01  NYCKLAR-TILL-DLI.                                                    
550000     03  W-IDARTNR-X.                                                     
560000         05  W-IDARTNR           PIC  S9(9)  COMP-3.                      
570000                                                                          
580000*    ---  DLI INPUT-OUTPUT AREA                                           
590000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
600000                                                                          
610000 01  DLI-IO-AREA.                                                         
620000     03  IO-AREA                 PIC X(100)  VALUE SPACE.                 
630000                                                                          
640000*    03  WLARTM01 -COPY WDK901        -RED IO-AREA.                       
660000     EJECT                                                                
670000                                                                          
680000 LINKAGE SECTION.                                                         
690000*                                                                         
700002*   -COPY W411ARTM                                                        
720000*    EJECT                                                                
730000                                                                          
740002*01  -COPY W0008      -PRE ARTM-                                          
760000     05  FILLER                  PIC X.                                   
770000     EJECT                                                                
780000*                                                                         
790000 PROCEDURE DIVISION  USING ARTM-W411ARTM ARTM-PCB.                        
800000                                                                          
810000     MOVE ARTM-IDARTNR-IN           TO ART-IDARTNR                        
850003     MOVE 0                         TO ART-KVOFFERT                       
860003     MOVE 0                         TO ART-KVOKS-BULK                     
870003     MOVE 0                         TO ART-KVOKS-DAG                      
880003     MOVE 0                         TO ART-KVOKS-VOR                      
890003     MOVE 0                         TO ART-KVPREAVB-BULK                  
900003     MOVE 0                         TO ART-KVPREAVB-DAG                   
910003     MOVE 0                         TO ART-KVPREAVB-VOR                   
920003     MOVE 0                         TO ART-KVPRERO-BULK                   
930003     MOVE 0                         TO ART-KVPRERO-DAG                    
940003     MOVE 1                         TO ART-RERF-ART                       
950003     MOVE 0                         TO ART-SUTPO-TOT                      
950005     MOVE SPACE                     TO ART-FILLER                         
990000                                                                          
000000     PERFORM IMS-ISRT-ARTM01                                              
010000     GOBACK.                                                              
020000     EJECT                                                                
030000                                                                          
040000 IMS-ISRT-ARTM01               SECTION.                                   
050000     SKIP2                                                                
060000     MOVE 'WLARTM01 '                    TO SSA1                          
070000     MOVE '  II'                         TO GODK-STATUSKODER              
080000     CALL CBLTDLI USING ISRT ARTM-PCB DLI-IO-AREA SSA1                    
090000     MOVE ARTM-STATUS-CODE               TO STATUS-WS                     
100000     PERFORM IMS-STATUSKONTROLL                                           
110000     .                                                                    
120000     SKIP2                                                                
130000                                                                          
140000 IMS-STATUSKONTROLL            SECTION.                                   
150000     SKIP2                                                                
160000     SET STATUS-IX             TO 1                                       
170000     SEARCH GODK-STATUS AT END CALL FELLOG                                
180000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
190000     END-SEARCH                                                           
200000     .                                                                    
