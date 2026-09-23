010000 ID DIVISION.                                                             
020000 PROGRAM-ID.     W4183A00.                                                
030000 AUTHOR.         OLSSON SUSANNE.                                          
040000 DATE-WRITTEN.   08/04/29.                                                
050000 DATE-COMPILED.                                                           
060000                                                                          
070000                                                                          
080000*    FUNKTION:                                                            
090000*        PROGRAMMET LÄSER IN EN FIL MED POSTER SOM SKALL UPPDATERA        
100000*        WDR4/HTYP4101.FILEN KOMMER FRÅN PROGRAM W4183900 DÄR MAN         
110000*        TAR FRAM FÄRDIGA RAPPORTER AV RETUR-AV-RETUR SOM SKALL           
120000*        BLI FAKTURERADE FÖR HANTERINGSKOSTNADEN.                         
130000*                                                                         
140000*        PROGRAMMET UPPDATERAR WDR4                                       
150000*                                                                         
150001*        BMP                                                              
150010*                                                                         
150100*    E'TRACKER 880053 DATED 2008-04-29                                    
150200*                                                                         
160000                                                                          
170000     SKIP3                                                                
180000 ENVIRONMENT DIVISION.                                                    
190000     SKIP2                                                                
200000 INPUT-OUTPUT SECTION.                                                    
210000                                                                          
220000 FILE-CONTROL.                                                            
230000     SKIP2                                                                
240000*          --- UPPDATERINGSRADER WDGX4102                                 
250000     SELECT W418AU                     ASSIGN TO W4183AD1.                
260000     EJECT                                                                
270000 DATA DIVISION.                                                           
280000     SKIP3                                                                
290000 FILE SECTION.                                                            
300000     SKIP3                                                                
310000 FD  W418AU                                                               
320000     RECORDING       F                                                    
330000     BLOCK CONTAINS  0.                                                   
340000                                                                          
350000*01  -COPY W418AU      -L.                                                
360000     EJECT                                                                
370000 WORKING-STORAGE SECTION.                                                 
380000                                                                          
390000 77  IDPGM                       PIC X(8)    VALUE 'W4183A00'.            
391001 77  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
392001 77  SPAR-IDDISTR                PIC S9(5)   VALUE +0   COMP-3.           
400000 01  CHKP-VAR.                                                            
410000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
420000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
430000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
440000     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
450000     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
460000     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
470000 77  JA                          PIC X       VALUE 'J'.                   
480000 77  NEJ                         PIC X       VALUE 'N'.                   
490000     SKIP2                                                                
500000 01  FELTEXT.                                                             
510000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
520000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
530000                                                                          
540000 77  W418AU-EOF-SW               PIC X       VALUE 'N'.                   
550000     88  END-OF-W418AU                       VALUE 'J'.                   
560000     EJECT                                                                
570000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
580000 01  FILLER REDEFINES DAGENS-DATUM.                                       
590000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
600000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
610000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
620000     EJECT                                                                
630000 01  DYNAMISKA-SUBPROGRAM.                                                
640000*                                                                         
650000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
660000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
670000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
680000     EJECT                                                                
690000*    --- PARAMETRAR TILL POSTSUM                                          
700000*                                                                         
710000*01  -COPY W0005   -PRE  POSTSUM-                                         
720000     EJECT                                                                
730000 01  IN-AREA-START               PIC X(24)   VALUE                        
740000                                             'IN-AREA-START'.             
750000     SKIP2                                                                
760000                                                                          
770000*01  AREA -COPY W418AU     -PRE IN-                                       
780000*                                                                         
790000     EJECT                                                                
800000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
810000     SKIP3                                                                
820000 01  NYCKLAR-TILL-DLI.                                                    
830001     03  W-WDGX4101-X.                                                    
840001         05 W-IDHTYP               PIC X(4)    VALUE '4101'.              
850001         05 W-IDDC                 PIC X(2)    VALUE SPACE.               
851001         05 W-IDDISTR              PIC S9(5)   VALUE ZERO COMP-3.         
852001         05 W-LOWVALUE             PIC X(21)   VALUE LOW-VALUE.           
860001                                                                          
861001     03 W-WDGX4102-X.                                                     
862001         05 W-IDKUNDNR             PIC S9(7)   VALUE ZERO COMP-3.         
863001         05 W-IDRAPP               PIC X(10)   VALUE SPACE.               
864001         05 W-IDKOLLI              PIC S9(5)   VALUE ZERO COMP-3.         
865001         05 W-IDARTNR              PIC S9(9)   VALUE ZERO COMP-3.         
870000     SKIP2                                                                
880000*    --- STATUS-KOD FRÅN IMS                                              
890000 01  STATUS-WS                   PIC XX.                                  
900000     88  SEGMENT-FINNS                       VALUE '  '.                  
910000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
920000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
930000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
940000     88  IMS-EJ-OK                           VALUE 'XD'.                  
950000     SKIP2                                                                
960000 01  GODK-STATUSKODER.                                                    
970000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
980000     SKIP3                                                                
990000 01  SSA1                        PIC X(64).                               
000000 01  SSA2                        PIC X(64).                               
010000     EJECT                                                                
020000*    --- IMS FUNKTIONSKODER                                               
030000*01  -COPY W0003                                                          
040000     EJECT                                                                
050000*    ---  DLI INPUT-OUTPUT AREA                                           
060000                                                                          
070000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4101'.                    
080000 01  DLI-IO-WDGX4101.                                                     
090000*    03  -COPY WDGX4101                                                   
100000     EJECT                                                                
110000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4102'.                    
120000 01  DLI-IO-WDGX4102.                                                     
130000*    03  -COPY WDGX4102                                                   
140000                                                                          
150000     EJECT                                                                
160000 LINKAGE SECTION.                                                         
170000                                                                          
180000*01  -COPY W0009   -PRE MSG-                                              
190000                                                                          
200000*01  -COPY W0008  -PRE 4101-                                              
210000     05  FILLER                  PIC X.                                   
220000     EJECT                                                                
230000 PROCEDURE DIVISION  USING MSG-PCB 4101-PCB.                              
240000 MAIN SECTION.                                                            
250000     ENTRY 'DLITCBL' USING MSG-PCB 4101-PCB.                              
260000                                                                          
270000     SKIP2                                                                
280000     PERFORM A-INIT                                                       
290000     PERFORM S01-LAES-W418AU                                              
300001     PERFORM UNTIL END-OF-W418AU                                          
340000                                                                          
350001       PERFORM B-BEHANDLA                                                 
390000                                                                          
400000       PERFORM S01-LAES-W418AU                                            
410000     END-PERFORM                                                          
430000                                                                          
440000     PERFORM Z-FINIT                                                      
450000                                                                          
460000     MOVE ZERO TO RETURN-CODE                                             
470000     GOBACK                                                               
480000     .                                                                    
490000     EJECT                                                                
500000 A-INIT SECTION.                                                          
510000     SKIP2                                                                
520000                                                                          
530000     PERFORM IMS-RESTART                                                  
540000                                                                          
550000     OPEN INPUT W418AU                                                    
560000                                                                          
571001     MOVE +0    TO CHKP-ANT                                               
580000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
590000     .                                                                    
600000     EJECT                                                                
600101 B-BEHANDLA SECTION.                                                      
600201                                                                          
600301     IF IN-IDDC    = SPAR-IDDC    AND                                     
600401        IN-IDDISTR = SPAR-IDDISTR                                         
600501       CONTINUE                                                           
600601     ELSE                                                                 
600901       MOVE IN-IDDC       TO W-IDDC                                       
601001                             SPAR-IDDC                                    
601101       MOVE IN-IDDISTR    TO W-IDDISTR                                    
601201                             SPAR-IDDISTR                                 
601301       PERFORM IMS-GU-WDGX4101                                            
601401     END-IF                                                               
601501                                                                          
601601     IF CHKP-ANT > CHKP-MAX                                               
601701       PERFORM X-TAG-CHECKPOINT                                           
601801     END-IF                                                               
601901                                                                          
602001     MOVE IN-IDKUNDNR     TO W-IDKUNDNR                                   
602101     MOVE IN-IDRAPP       TO W-IDRAPP                                     
602201     MOVE IN-IDKOLLI      TO W-IDKOLLI                                    
602301     MOVE IN-IDARTNR      TO W-IDARTNR                                    
602401     PERFORM IMS-GHNP-WDGX4102                                            
602501     IF SEGMENT-FINNS                                                     
602601       IF 4102-FLFAKT = NEJ                                               
602901         MOVE JA          TO 4102-FLFAKT                                  
603001         PERFORM IMS-REPL-WDGX4102                                        
603201         ADD +1 TO CHKP-ANT                                               
603301       END-IF                                                             
603401     END-IF                                                               
603501     .                                                                    
604001     EJECT                                                                
610000 Z-FINIT SECTION.                                                         
620000                                                                          
630000                                                                          
640000     CLOSE W418AU                                                         
650000     SKIP2                                                                
660000     MOVE 'S' TO POSTSUM-OPKOD                                            
670000     CALL POSTSUM USING POSTSUM-PARM                                      
680000     .                                                                    
690000     EJECT                                                                
700000 S01-LAES-W418AU  SECTION.                                                
710000     SKIP2                                                                
720000     READ W418AU INTO IN-AREA                                             
730000     AT END                                                               
750000        SET END-OF-W418AU TO TRUE                                         
760000                                                                          
770000     NOT AT END                                                           
780000        MOVE 'W418AU' TO POSTSUM-FDNAMN                                   
790000        MOVE 'W4183AD1' TO POSTSUM-DDNAMN2                                
800001        MOVE IN-IDHTYP TO POSTSUM-TRANSTYP                                
810000        CALL POSTSUM USING POSTSUM-PARM                                   
840000     END-READ                                                             
850000     .                                                                    
860000     EJECT                                                                
870000 X-TAG-CHECKPOINT   SECTION.                                              
880000                                                                          
910000     PERFORM IMS-CHECKPOINT                                               
920000     MOVE ZERO TO CHKP-ANT                                                
931001     PERFORM IMS-GU-WDGX4101                                              
940000     .                                                                    
950000     EJECT                                                                
960000* --- IMS SEKTIONER ---                                                   
970000                                                                          
980000     EJECT                                                                
990000 IMS-GET-WDGX4101 SECTION.                                                
000000                                                                          
010001     STRING 'WDR401  (WDGXKEY  =' W-WDGX4101-X ')'                        
020000          DELIMITED BY SIZE INTO SSA1                                     
030000     MOVE '  GE' TO GODK-STATUSKODER                                      
040000     CALL CBLTDLI USING GNP 4101-PCB DLI-IO-WDGX4101 SSA1                 
050000     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
060000     PERFORM IMS-STATUSKONTROLL                                           
070000     .                                                                    
080000     EJECT                                                                
081001 IMS-GU-WDGX4101 SECTION.                                                 
082001                                                                          
083001     STRING 'WDR401  (WDGXKEY  =' W-WDGX4101-X ')'                        
084001          DELIMITED BY SIZE INTO SSA1                                     
085001     MOVE '  GE' TO GODK-STATUSKODER                                      
086001     CALL CBLTDLI USING GU 4101-PCB DLI-IO-WDGX4101 SSA1                  
087001     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
088001     PERFORM IMS-STATUSKONTROLL                                           
089001     .                                                                    
089101     EJECT                                                                
090001 IMS-GHNP-WDGX4102 SECTION.                                               
100000                                                                          
110001     STRING 'WDGX4102(KY4102   =' W-WDGX4102-X ')'                        
120000          DELIMITED BY SIZE INTO SSA1                                     
130000     MOVE '  GE' TO GODK-STATUSKODER                                      
140000     CALL CBLTDLI USING GHNP 4101-PCB DLI-IO-WDGX4102 SSA1                
150000     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
160000     PERFORM IMS-STATUSKONTROLL                                           
170000     .                                                                    
180000     SKIP3                                                                
190000 IMS-REPL-WDGX4102 SECTION.                                               
200000                                                                          
210000     MOVE '  ' TO GODK-STATUSKODER                                        
220000     CALL CBLTDLI USING REPL 4101-PCB DLI-IO-WDGX4102                     
230000     MOVE 4101-STATUS-CODE TO STATUS-WS                                   
240000     PERFORM IMS-STATUSKONTROLL                                           
250000     .                                                                    
260000     EJECT                                                                
270000 IMS-RESTART SECTION.                                                     
280000     SKIP2                                                                
290000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
300000     MOVE '  ' TO GODK-STATUSKODER                                        
310000     CALL CBLTDLI USING XRST MSG-PCB                                      
320000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
330000                        CHKP-AREA-LENGTH CHKP-AREA                        
340000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
350000     PERFORM IMS-STATUSKONTROLL                                           
360000     .                                                                    
370000     SKIP3                                                                
380000 IMS-CHECKPOINT SECTION.                                                  
390000     SKIP2                                                                
400000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
410000     MOVE '  XD' TO GODK-STATUSKODER                                      
420000     CALL CBLTDLI USING CHKP MSG-PCB                                      
430000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
440000                        CHKP-AREA-LENGTH CHKP-AREA                        
450000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
460000     PERFORM IMS-STATUSKONTROLL                                           
470000                                                                          
480000     IF IMS-EJ-OK                                                         
490000       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
500000       DISPLAY FELTEXT                                                    
510000       CALL FELLOG                                                        
520000     END-IF                                                               
530000     .                                                                    
540000     EJECT                                                                
550000 IMS-STATUSKONTROLL SECTION.                                              
560000     SKIP2                                                                
570000     SET STATUS-IX TO 1                                                   
580000     SEARCH GODK-STATUS                                                   
590000       AT END                                                             
600000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
610000           DELIMITED BY SIZE INTO FELTEXT                                 
620000         DISPLAY FELTEXT                                                  
630000         CALL FELLOG                                                      
640000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
650000         CONTINUE                                                         
660000     END-SEARCH                                                           
670000     .                                                                    
