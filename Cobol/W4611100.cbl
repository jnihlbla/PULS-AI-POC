011011 ID DIVISION.                                                             
030000 PROGRAM-ID.     W4611100.                                                
040000 AUTHOR.         LARS THELL      (MG).                                    
050000 DATE-WRITTEN.   91/02/04.                                                
060000                                                                          
060100                                                                          
070000     REMARKS.                                                             
090000*    FUNKTION:                                                            
100007*        KOLLAR MOT WDB2 VILKA DISTRIKT SOM SKALL FÅ VIPS TRANSAR         
110000*                                                                         
120007*        PROGRAMMET LÄSER     WLGMTA (WDB2)                               
130000*                                                                         
140000*    ABENDKODER:                                                          
150000*        U0016 -  . . . . RETURKOD FRÅN SORTERING                         
160000*                                                                         
170000                                                                          
180000     SKIP3                                                                
190000 ENVIRONMENT DIVISION.                                                    
200000     SKIP2                                                                
210000 INPUT-OUTPUT SECTION.                                                    
220000                                                                          
230000 FILE-CONTROL.                                                            
240000     SKIP2                                                                
250000*          --- VIPS TRANSAR IN                                            
260000     SELECT W46111                     ASSIGN TO W46111D1.                
270000     SKIP2                                                                
280000*          --- VIPS TRANSAR UT                                            
290000     SELECT W46112                     ASSIGN TO W46111D2.                
300000     SKIP2                                                                
310000*          --- SORTERINGSFIL                                              
320000     SELECT SORTFIL                    ASSIGN TO W46111DS.                
330000     EJECT                                                                
340000 DATA DIVISION.                                                           
350000     SKIP3                                                                
360000 FILE SECTION.                                                            
370000     SKIP3                                                                
380000 FD  W46111                                                               
390000     LABEL RECORD    STANDARD                                             
400000     RECORDING       V                                                    
410000     BLOCK CONTAINS  0.                                                   
420000     SKIP2                                                                
430001*01  -COPY W461S013        -L.                                            
440000                                                                          
450000                                                                          
460000     SKIP3                                                                
470000 FD  W46112                                                               
480000     LABEL RECORD    STANDARD                                             
490000     RECORDING       V                                                    
500000     BLOCK CONTAINS  0.                                                   
510000     SKIP2                                                                
520001*01  POST -COPY W461S013   -PRE  W46112-  -L.                             
530000                                                                          
540000     SKIP3                                                                
550000 SD  SORTFIL                                                              
560000     RECORDING V                                                          
570000     SKIP2                                                                
580000 01  SORT-POST.                                                           
590000    03   SORT-IDDISTR            PIC  9(5).                               
591008    03   SORT-IDKUNDNR           PIC  9(7).                               
600001*03  -COPY W461S013        -L.                                            
610000     EJECT                                                                
620000 WORKING-STORAGE SECTION.                                                 
630000     SKIP2                                                                
630111                                                                          
631011*    -- CHECKED BY WY2000                                                 
640000 77  IDPGM                       PIC X(8)    VALUE 'W4611100'.            
650000 77  JA                          PIC X       VALUE 'J'.                   
660000 77  NEJ                         PIC X       VALUE 'N'.                   
670000 77  W-OLD-IDDISTR               PIC 9(5).                                
680000                                                                          
690000 77  W46111-EOF-SW               PIC X       VALUE 'N'.                   
700000     88  END-OF-W46111                       VALUE 'J'.                   
710000                                                                          
720000 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
730000     88  END-OF-SORTFIL                      VALUE 'J'.                   
740000     EJECT                                                                
750000 01  DAGENS-DATUM.                                                        
760000     03  DAGENS-DATUM-AAR        PIC X(2)    VALUE SPACE.                 
770000     03  DAGENS-DATUM-MAANAD     PIC X(2)    VALUE SPACE.                 
780000     03  DAGENS-DATUM-DAG        PIC X(2)    VALUE SPACE.                 
790000     EJECT                                                                
800000****************************************************************          
810000*                DISTRIKTSTEST-COPYTEXT                                   
820000****************************************************************          
830000*                                                                         
840000 01  FILLER         PIC X(16)    VALUE 'DISTRIKTSAREA   '.                
850000     SKIP2                                                                
860000 01  TEST-IDDISTR   PIC 9(5)    COMP-3.                                   
870000     SKIP2                                                                
880004*01  FILLER        -COPY WWDIS130   -RED TEST-IDDISTR.                    
890000     EJECT                                                                
900000 01  DYNAMISKA-SUBPROGRAM.                                                
910000*                                                                         
920000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
930000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
940000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
950000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
951004     03  W460DIS1                PIC X(8)    VALUE 'W460DIS1'.            
960000     SKIP2                                                                
970000*    --- PARAMETRAR TILL ABEND                                            
980000                                                                          
990000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010000     EJECT                                                                
020000*    --- PARAMETRAR TILL POSTSUM                                          
030000*                                                                         
040001*01  -COPY W0005      -PRE  POSTSUM-                                      
050000     EJECT                                                                
051004*    --- PARAMETRAR TILL W460DIS1                                         
052004*                                                                         
053004*01  -COPY W460DIS1                                                       
054004     EJECT                                                                
060000 01  W46111-AREA-START           PIC X(24)   VALUE                        
070000                                 'W46111-AREA-START  '.                   
080000     SKIP2                                                                
090000 01  W46111-AREA.                                                         
100000     03  W46111-IDDISTR          PIC S9(5)  VALUE ZERO COMP-3.            
101009     03  W46111-IDKUNDNR         PIC S9(7)  VALUE ZERO COMP-3.            
110009     03  FILLER                  PIC X(14).                               
120000     03  W46111-IDPTYP           PIC X(3).                                
130015     03  FILLER                  PIC X(170).                              
140000     EJECT                                                                
150000 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
160000                                  'SORTWS-AREA-START  '.                  
170000     SKIP2                                                                
180000 01  SORTWS-AREA.                                                         
190000     03  SORTWS-IDDISTR          PIC 9(5).                                
191008     03  SORTWS-IDKUNDNR         PIC 9(7).                                
200000     03  SORTWS-VIPSPOST.                                                 
210002      05 FILLER                  PIC X(21).                               
220000      05 SORTWS-IDPTYP           PIC X(3).                                
230015      05 FILLER                  PIC X(170).                              
240000     EJECT                                                                
250000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
260000*                                                                         
270000     EJECT                                                                
280000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
290000     SKIP3                                                                
300000 01  NYCKLAR-TILL-DLI.                                                    
301007     03  W-IDGMT-KEY.                                                     
302008         05  W-IDDISTR           PIC S9(5)      COMP-3.                   
303008         05  W-IDKUNDNR          PIC S9(7)      COMP-3.                   
304007                                                                          
330000     SKIP2                                                                
340000*    --- STATUS-KOD FRÅN IMS                                              
350000 01  STATUS-WS                   PIC XX.                                  
360000     88  SEGMENT-FINNS                       VALUE '  '.                  
370000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
380000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
390000     SKIP2                                                                
400000 01  GODK-STATUSKODER.                                                    
410000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
420000     SKIP3                                                                
430000 01  SSA1                        PIC X(64).                               
440000 01  SSA2                        PIC X(64).                               
450000     EJECT                                                                
460000*    --- IMS FUNKTIONSKODER                                               
470001*01  -COPY W0003                                                          
480000     EJECT                                                                
490000*    ---  DLI INPUT-OUTPUT AREA                                           
500000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
510000     SKIP3                                                                
520000 01  DLI-IO-AREA.                                                         
550016     03  WLGMTA01.                                                        
560007*        05  -COPY WDB201                                                 
570000     EJECT                                                                
580000 LINKAGE SECTION.                                                         
590000                                                                          
600000     EJECT                                                                
610007*01  -COPY W0008      -PRE GMTA-                                          
620000     05  FILLER                  PIC X.                                   
630000     EJECT                                                                
640007 PROCEDURE DIVISION  USING GMTA-PCB.                                      
650007     ENTRY 'DLITCBL' USING GMTA-PCB.                                      
660000                                                                          
670000     SKIP2                                                                
680000     PERFORM A-INIT                                                       
690000                                                                          
700000     SORT SORTFIL ASCENDING KEY SORT-IDDISTR                              
710000                  INPUT PROCEDURE B-BEHANDLA-INFIL                        
720000                  OUTPUT PROCEDURE C-KOLLA-IDDISTR-SKRIV-UTFIL            
730000                                                                          
740000     IF SORT-RETURN NOT = 0                                               
750000       DISPLAY 'RETURKOD ' SORT-RETURN ' FRÅN SORT'                       
760000       PERFORM S99-ABEND                                                  
770000     ELSE                                                                 
780000       PERFORM Z-FINIT                                                    
790000                                                                          
800000       MOVE ZERO TO RETURN-CODE                                           
810000       GOBACK                                                             
820000     END-IF                                                               
830000                                                                          
840000     .                                                                    
850000     EJECT                                                                
860000 A-INIT SECTION.                                                          
870000                                                                          
880000     OPEN INPUT  W46111                                                   
890000                                                                          
900000     OPEN OUTPUT W46112                                                   
910000     SKIP2                                                                
920000     ACCEPT DAGENS-DATUM        FROM DATE                                 
930000     MOVE IDPGM                 TO POSTSUM-PROGNAMN                       
940000     .                                                                    
950000     EJECT                                                                
960000 B-BEHANDLA-INFIL   SECTION.                                              
970000                                                                          
980000     PERFORM S01-LAES-W46111                                              
990000     PERFORM UNTIL END-OF-W46111                                          
000000         MOVE W46111-IDDISTR   TO  SORTWS-IDDISTR                         
001008         MOVE W46111-IDKUNDNR  TO  SORTWS-IDKUNDNR                        
010000         MOVE W46111-AREA      TO  SORTWS-VIPSPOST                        
020000         PERFORM S31-SORT-RELEASE                                         
030000         PERFORM S01-LAES-W46111                                          
040000     END-PERFORM                                                          
050000     .                                                                    
060000     EJECT                                                                
070000 C-KOLLA-IDDISTR-SKRIV-UTFIL  SECTION.                                    
080000                                                                          
090000     MOVE ZERO                 TO  W-OLD-IDDISTR                          
100000     PERFORM S32-SORT-RETURN                                              
110000     PERFORM UNTIL END-OF-SORTFIL                                         
120000         IF SORTWS-IDDISTR     =   W-OLD-IDDISTR                          
130000             CONTINUE                                                     
140000          ELSE                                                            
150000             MOVE SORTWS-IDDISTR  TO  W-IDDISTR                           
151007             MOVE SORTWS-IDKUNDNR TO  W-IDKUNDNR                          
160007             PERFORM IMS-GU-GMTA01                                        
170000             MOVE SORTWS-IDDISTR  TO  W-OLD-IDDISTR                       
180000         END-IF                                                           
190000                                                                          
200004         MOVE SORTWS-IDDISTR   TO  TEST-IDDISTR DIS1-IDDISTR              
201004                                                                          
202004         CALL W460DIS1 USING DIS1-W460DIS1                                
203004                                                                          
210007         IF GMT-FLNC = JA OR DIS1-KDSVAR = JA                             
220004           OR DIS130-NOAC                                                 
230000             PERFORM S11-SKRIV-W46112                                     
240000         END-IF                                                           
250000         PERFORM S32-SORT-RETURN                                          
260000     END-PERFORM                                                          
270000     .                                                                    
280000     EJECT                                                                
290000 Z-FINIT SECTION.                                                         
300000     CLOSE W46111                                                         
310000           W46112                                                         
320000     SKIP2                                                                
330000     MOVE 'S' TO POSTSUM-OPKOD                                            
340000     CALL POSTSUM USING POSTSUM-PARM                                      
350000     .                                                                    
360000     EJECT                                                                
370000 S01-LAES-W46111  SECTION.                                                
380000     SKIP2                                                                
390000     READ W46111 INTO W46111-AREA                                         
400000     AT END                                                               
410000        SET END-OF-W46111 TO TRUE                                         
420000                                                                          
430000     NOT AT END                                                           
440000        MOVE 'W46111'          TO POSTSUM-FDNAMN                          
450000        MOVE 'W46111D1'        TO POSTSUM-DDNAMN2                         
460000        MOVE W46111-IDPTYP     TO POSTSUM-TRANSTYP                        
470000        CALL POSTSUM           USING POSTSUM-PARM                         
480000     END-READ                                                             
490000     .                                                                    
500000     EJECT                                                                
510000 S11-SKRIV-W46112 SECTION.                                                
520000     SKIP2                                                                
530000     WRITE W46112-POST FROM SORTWS-VIPSPOST                               
540000                                                                          
550000     MOVE SORTWS-IDPTYP        TO POSTSUM-TRANSTYP                        
560000     MOVE 'W46112'             TO POSTSUM-FDNAMN                          
570000     MOVE 'W46112D2'           TO POSTSUM-DDNAMN2                         
580000     CALL POSTSUM USING        POSTSUM-PARM                               
590000     .                                                                    
600000     EJECT                                                                
610000 S31-SORT-RELEASE  SECTION.                                               
620000     SKIP2                                                                
630000     RELEASE SORT-POST FROM SORTWS-AREA                                   
640000     .                                                                    
650000     EJECT                                                                
660000 S32-SORT-RETURN  SECTION.                                                
670000     SKIP2                                                                
680000     RETURN SORTFIL INTO SORTWS-AREA                                      
690000     AT END                                                               
700000         SET END-OF-SORTFIL TO TRUE                                       
710000     .                                                                    
720000     EJECT                                                                
730000 S99-ABEND SECTION.                                                       
740000     SKIP2                                                                
750000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
760000     .                                                                    
770000     EJECT                                                                
780000* --- IMS SEKTIONER ---                                                   
790000     SKIP3                                                                
800000     EJECT                                                                
801107 IMS-GU-GMTA01 SECTION.                                                   
802007     SKIP2                                                                
802110     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-KEY ')'                         
802210          DELIMITED BY SIZE INTO SSA1                                     
805010     MOVE '  '                  TO GODK-STATUSKODER                       
806007     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA SSA1                      
807010     MOVE GMTA-STATUS-CODE      TO STATUS-WS                              
808007     PERFORM IMS-STATUSKONTROLL                                           
809007     .                                                                    
809107     EJECT                                                                
900000 IMS-STATUSKONTROLL SECTION.                                              
910000     SKIP2                                                                
920000     SET STATUS-IX TO 1                                                   
930000     SEARCH GODK-STATUS                                                   
940000       AT END CALL FELLOG                                                 
950000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
960000     END-SEARCH                                                           
970000     .                                                                    
