000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W9043100.                                                 
000300 AUTHOR.        ERIK RINGQVIST.                                           
000400 DATE-WRITTEN.  MAJ 1984.                                                 
000500*    REMARKS.                                                             
000600*    FUNKTION.                                                            
000700*        TP-PROGRAM FÖR REGISTRERING AV FÄLTET KDARTHNT                   
000800*        I SEGMENT 13 I ARTIKELREGISTRET.                                 
000900*                                                                         
001000*     +  STARTAR EV DISPATCH FÖR UPPDATERING AV URSPRUNG PÅ               
001100*        INLEVERANSREGISTRET.                                             
001200*                                                                         
120000*    INDATA.                                                              
130036*        TRANSAKTION: W90431T                                             
140036*        MID:         W90431I1                                            
150000*    UTDATA.                                                              
160036*        MOD:         W90431O1                                            
170000*    SUBPROGRAM.                                                          
180000*        FELLOG                                                           
182022*        W006KOM  (DISPATCH)                                              
190000*    SKIP3                                                                
200000 ENVIRONMENT DIVISION.                                                    
210000*                                                                         
220000 DATA DIVISION.                                                           
230000     EJECT                                                                
240000 WORKING-STORAGE SECTION.                                                 
250000                                                                          
250121                                                                          
251021*    -- CHECKED BY WY2000                                                 
260036 77    PROGRAM-NAMN          PIC X(8)    VALUE 'W9043100'.                
270000 77    JA                    PIC X       VALUE 'J'.                       
280000 77    NEJ                   PIC X       VALUE 'N'.                       
280123 77  LNG-P-TO-P-PREFIX           PIC S9(4) COMP SYNC   VALUE +17.         
280223 77  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
280323 77  DAGENS-TID                  PIC 9(8)   VALUE ZERO.                   
280425 77  WS-FEL3                     PIC X(1)   VALUE 'N'.                    
280531 77  WS-IDARTNR-NUM9             PIC 9(9)   VALUE ZERO.                   
280623*                                                                         
281004                                                                          
290004 77  FEL-FINNS-SW            PIC X       VALUE 'N'.                       
291005     88  FEL-FINNS                       VALUE 'J'.                       
291105     88  FEL-SAKNAS                      VALUE 'N'.                       
291204                                                                          
292004 77  INDATA-FINNS-SW         PIC X       VALUE 'J'.                       
293005     88  INDATA-SAKNAS                   VALUE 'N'.                       
293105     88  INDATA-FINNS                    VALUE 'J'.                       
294004                                                                          
310000 77    MAX-RAD-IX-PLUS       PIC S9(9)   VALUE +10   COMP SYNC.           
320000 77    RAD-IX                PIC S9(9)   VALUE +1    COMP SYNC.           
330000 77    SPRAK-IX              PIC S9(9)   VALUE +1    COMP SYNC.           
340000     SKIP3                                                                
341032*      --- VALID IDDC CODES                                               
342032*                                                                         
343032*01    -COPY WWDCKONS                                                     
344032       EJECT                                                              
350000 01    DYNAMISKA-SUBPROGRAM.                                              
360000   03    CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
370000   03    FELLOG              PIC X(8)    VALUE 'FELLOG'.                  
370122   03    W400ARTU            PIC X(8)    VALUE 'W400ARTU'.                
371022   03    W006KOM             PIC X(8)    VALUE 'W006KOM '.                
372022* - - - - - - - - - - - - - - - - - - -  NYCKLAR TILL ARTU                
373022 01  FILLER                      PIC X(16)  VALUE 'STARTARTU'.            
374022*    -COPY W400ARTU                                                       
375022     EJECT                                                                
376022* - - - - - - - - - - - - - - - - - - -  NYCKLAR TILL DLI                 
380000 01    NYCKLAR-TILL-DLI.                                                  
390000   03    W-IDARTNR-X.                                                     
400000     05    W-IDARTNR         PIC S9(9)   VALUE ZERO  COMP-3.              
410000   03    W-KDCLAGER-X.                                                    
420000     05    W-KDCLAGER        PIC S9(1)   VALUE ZERO  COMP-3.              
430000     SKIP3                                                                
440000 01    W-KDARTHNT            PIC 9(6).                                    
450000 01    FILLER REDEFINES W-KDARTHNT.                                       
460000   03    W-KDARTHNT-V        PIC 9(3).                                    
470000   03    W-KDARTHNT-H        PIC 9(3).                                    
480000     EJECT                                                                
490000 01    MEDDELANDE.                                                        
500000*                                                                         
510000   03    RETT1.                                                           
520000     05    FILLER            PIC X(40)                                    
530000               VALUE 'REGISTRERING UTFÖRD      '.                         
540000     05    FILLER            PIC X(40)                                    
550000               VALUE 'REGISTRATION COMPLETED   '.                         
560000   03    FILLER REDEFINES RETT1.                                          
570000     05    RETT-1            PIC X(40)  OCCURS 2.                         
580000*                                                                         
590000   03    FEL1.                                                            
600000     05    FILLER            PIC X(40)                                    
610000               VALUE 'ARTIKELN FINNS EJ I ARTIKELREGISTRET   '.           
620000     05    FILLER            PIC X(40)                                    
630000               VALUE 'PART NO NOT IN THE DATABASE            '.           
640000   03    FILLER REDEFINES FEL1.                                           
650000     05    FEL-1             PIC X(40)  OCCURS 2.                         
660000*                                                                         
670000   03    FEL2.                                                            
680000     05    FILLER            PIC X(40)                                    
690000               VALUE 'UPPLYSTA FÄLT FEL                      '.           
700000     05    FILLER            PIC X(40)                                    
710000               VALUE 'LIGHTENED FIELDS WRONG                 '.           
720000   03    FILLER REDEFINES FEL2.                                           
730000     05    FEL-2             PIC X(40)  OCCURS 2.                         
730125*                                                                         
731025   03    FEL3.                                                            
732025     05    FILLER            PIC X(40)                                    
733025               VALUE 'UPPDATERING EJ TILLÅTEN/URSPRUNG FINNS '.           
734025     05    FILLER            PIC X(40)                                    
735025               VALUE 'UPDATE NOT ALLOWED/ORIGIN ALREADY EXIST'.           
736025   03    FILLER REDEFINES FEL3.                                           
737025     05    FEL-3             PIC X(40)  OCCURS 2.                         
740000     EJECT                                                                
750000****************************************************************          
760000*                                                                         
770000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
780000*                                                                         
790000 01    FILLER                PIC X(8)    VALUE 'MFS-WS  '.                
800000                                                                          
810036*01    MID -COPY W90431I1                                                 
830000     EJECT                                                                
840000*01    -COPY WMSGAREA                                                     
860000     EJECT                                                                
870036*  03    W90431O1 -COPY W90431O1         -RED MSG-AREA.                   
890000     EJECT                                                                
900000*01      -COPY WMFSAREA                                                   
920000     EJECT                                                                
921022 01  FILLER                   PIC X(16)  VALUE 'KOM-MSG-IO-AREA '.        
922022 01  KOM-MSG-IO-AREA.                                                     
923022*03  -COPY WMSGKOM                                                        
924022     EJECT                                                                
925022                                                                          
926022 01  P-TO-P-SW.                                                           
927022     02     P-TO-P-KVLL             PIC S9(4) COMP SYNC.                  
928022     02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.            
929022     02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.            
929122     02     P-TO-P-KDTRANS          PIC X(8).                             
929222     02     P-TO-P-IDTRANS          PIC X(4).                             
929322     02     P-TO-P-KDMFSFOR         PIC X(1).                             
929422     02     P-TO-P-DATA             PIC X(1000).                          
929522     EJECT                                                                
929622                                                                          
929722 01      FILLER                  PIC X(24)   VALUE                        
929822                                 'MOD619B-MID-W6I19B01'.                  
929922     SKIP2                                                                
930022     -COPY W6I19B01 -PRE MOD619B-                                         
930122     EJECT                                                                
930222*-----------------------------------------------------------              
931000 01    IMS-WS.                                                            
940000   03    FILLER              PIC X(8)    VALUE 'IMS-WS  '.                
950000                                                                          
960000*                            *** STATUSKOD FRÅN IMS                       
970000   03    STATUS-WS           PIC XX.                                      
980000     88    SEGMENT-FINNS               VALUE '  '.                        
990000     88    SEGMENT-SAKNAS              VALUE 'GE'.                        
000000                                                                          
010000   03    GODK-STATUSKODER.                                                
020000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
030000                                                                          
040000 01    SSA1                  PIC X(32).                                   
050000 01    SSA2                  PIC X(32).                                   
060000 01    SSA3                  PIC X(32).                                   
070000                                                                          
080000*                            *** IMS FUNKTIONSKODER                       
090000*01      -COPY W0003                                                      
100000     EJECT                                                                
110000*                            *** DLI INPUT-OUTPUT AREA                    
120001 01  DLI-IO-AREA.                                                         
210019*  03  -COPY WDK611 -PRE ARTC-.                                           
220001     EJECT                                                                
230000 LINKAGE SECTION.                                                         
240000*01      -COPY W0009     -PRE MSG-                                        
250000     EJECT                                                                
260022*01      -COPY W0009     -PRE DISP-                                       
270022     EJECT                                                                
290001*01      -COPY W0008     -PRE ARTC-                                       
300001      05 FILLER              PIC X.                                       
310001     EJECT                                                                
311022*    PCB'ER FÖR SUBPGM                                                    
312022                                                                          
313022 01 KOM-KOMA-PCB         PIC X.                                           
314022     EJECT                                                                
320022 PROCEDURE DIVISION USING MSG-PCB DISP-PCB ARTC-PCB KOM-KOMA-PCB.         
330022     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB ARTC-PCB KOM-KOMA-PCB.        
340000                                                                          
350000 STYR SECTION.                                                            
360000     PERFORM IMS-GET-MSG                                                  
370000     IF SEGMENT-FINNS                                                     
380000       PERFORM A-INIT-SPARA-INPUT                                         
390036       IF MFS-IDTRANS = '9431'                                            
400000         PERFORM B-KONTROLLERA-INPUT                                      
410000                                                                          
420004         IF INDATA-FINNS  AND  FEL-SAKNAS                                 
430010           PERFORM C-KONTROLLERA-MOT-ARTC                                 
440000                                                                          
450004           IF FEL-SAKNAS                                                  
460010             PERFORM F-UPPDATERA-ARTC                                     
480000           ELSE                                                           
490026             IF WS-FEL3 = JA                                              
490126               MOVE FEL-3 (SPRAK-IX) TO MOD-MESSAGE-RAD1                  
490226             ELSE                                                         
490326               MOVE FEL-1 (SPRAK-IX) TO MOD-MESSAGE-RAD1                  
490426             END-IF                                                       
510000           END-IF                                                         
520000         ELSE                                                             
531026           MOVE FEL-2 (SPRAK-IX) TO MOD-MESSAGE-RAD1                      
550000         END-IF                                                           
560000       ELSE                                                               
571038         CONTINUE                                                         
580000       END-IF                                                             
590100       COMPUTE MSG-KVLL = LENGTH OF MOD-W90431O1 + 4                      
600000       PERFORM IMS-ISRT-MSG                                               
610000     END-IF                                                               
620000                                                                          
630000     MOVE ZERO TO RETURN-CODE                                             
640000     GOBACK                                                               
650000                                                                          
660000     .                                                                    
670000     EJECT                                                                
680000 A-INIT-SPARA-INPUT SECTION.                                              
690000                                                                          
700000     IF MSG-DUBBLA-TRANSKODER                                             
710036       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90431I1                 
720000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
730000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
740000     ELSE                                                                 
750036       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W90431I1                 
760000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
770000       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
780000     END-IF                                                               
780129                                                                          
782030     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
790000                                                                          
800000     MOVE LOW-VALUE       TO MSG-AREA                                     
810039     MOVE 'W90431O1'      TO MFS-IDMOD                                    
820036     MOVE '9431'          TO MOD-IDTRANS                                  
830000     MOVE MFS-RENSA-FAELT TO MOD-MESSAGE-RAD1                             
840000                             MOD-MESSAGE-RAD23                            
850000                                                                          
860000     IF ENGLISH-TEXT                                                      
870000       MOVE +2 TO SPRAK-IX                                                
880000       MOVE +2 TO W-KDCLAGER                                              
890000     ELSE                                                                 
900000       MOVE +1 TO SPRAK-IX                                                
910000       MOVE +1 TO W-KDCLAGER                                              
920000     END-IF                                                               
921023     ACCEPT DAGENS-DATUM       FROM DATE                                  
922023     ACCEPT DAGENS-TID         FROM TIME                                  
930000                                                                          
940000     .                                                                    
950000     EJECT                                                                
960000 B-KONTROLLERA-INPUT SECTION.                                             
970000                                                                          
980004     MOVE +1            TO RAD-IX                                         
981004     SET  FEL-SAKNAS    TO TRUE                                           
982004     SET  INDATA-SAKNAS TO TRUE                                           
983004                                                                          
990000     PERFORM UNTIL RAD-IX > MAX-RAD-IX-PLUS                               
000000       IF MID-IDARTNR   (RAD-IX) NOT = ALL '+'                            
010000         IF MID-IDARTNR (RAD-IX) NUMERIC                                  
020000           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-ATTR (RAD-IX)          
030000         ELSE                                                             
040000           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDARTNR-ATTR (RAD-IX)          
050004           SET  FEL-FINNS TO TRUE                                         
060000         END-IF                                                           
070000       END-IF                                                             
080000                                                                          
210040       IF MID-KDARTHNT-H (RAD-IX) NOT = ALL '+'                           
220040         IF MID-KDARTHNT-H (RAD-IX) NUMERIC                               
230040         AND MID-IDARTNR (RAD-IX)   NUMERIC                               
240040           MOVE MFS-NUM-FAELT-RAETT                                       
250040                              TO MOD-KDARTHNT-H-ATTR (RAD-IX)             
251040           SET  INDATA-FINNS  TO TRUE                                     
260040         ELSE                                                             
270040           MOVE MFS-NUM-FAELT-FEL                                         
280040                              TO MOD-KDARTHNT-H-ATTR (RAD-IX)             
281040           SET  FEL-FINNS     TO TRUE                                     
300040         END-IF                                                           
310040       END-IF                                                             
311022                                                                          
320000                                                                          
330004       ADD +1 TO RAD-IX                                                   
340000     END-PERFORM                                                          
350000                                                                          
360000     .                                                                    
370000     EJECT                                                                
380010 C-KONTROLLERA-MOT-ARTC      SECTION.                                     
390000                                                                          
400025     MOVE NEJ TO WS-FEL3                                                  
401025     MOVE +1 TO RAD-IX                                                    
410000     PERFORM UNTIL RAD-IX > MAX-RAD-IX-PLUS                               
420000                                                                          
430004       IF MID-IDARTNR (RAD-IX) NUMERIC                                    
440000         MOVE MID-IDARTNR (RAD-IX) TO W-IDARTNR                           
510007*                                                                         
520019         PERFORM IMS-GU-ARTC                                              
530001         IF SEGMENT-SAKNAS                                                
540001           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR (RAD-IX)            
541004           SET  FEL-FINNS     TO TRUE                                     
541128** FÅR BARA UPPDATERA URSPRUNG OM URSPRUNG ÄR SPACE                       
541228** FÖRUTOM MED PF11                                                       
542022         ELSE                                                             
542128           IF NOT MFS-UPDATE                                              
546137             CONTINUE                                                     
547027           END-IF                                                         
560001         END-IF                                                           
570000       END-IF                                                             
580000                                                                          
590000       ADD +1 TO RAD-IX                                                   
600000     END-PERFORM                                                          
610000                                                                          
620000     .                                                                    
630000     EJECT                                                                
640010 F-UPPDATERA-ARTC    SECTION.                                             
650000                                                                          
660000     MOVE +1 TO RAD-IX                                                    
670000     PERFORM UNTIL RAD-IX > MAX-RAD-IX-PLUS                               
680000                                                                          
690001       IF MID-IDARTNR (RAD-IX) NUMERIC                                    
700000         MOVE MID-IDARTNR (RAD-IX) TO W-IDARTNR                           
710031                                      WS-IDARTNR-NUM9                     
870019         PERFORM IMS-GU-ARTC                                              
880001         IF SEGMENT-FINNS                                                 
890019           MOVE ARTC-CLAG-KDARTHNT TO W-KDARTHNT                          
930040           IF MID-KDARTHNT-H     (RAD-IX) NUMERIC                         
940040             MOVE MID-KDARTHNT-H (RAD-IX) TO W-KDARTHNT-H                 
950040           END-IF                                                         
960019           MOVE  W-KDARTHNT TO ARTC-CLAG-KDARTHNT                         
970001           PERFORM IMS-REPLACE-ARTC                                       
980001         ELSE                                                             
990001           CALL FELLOG                                                    
000001         END-IF                                                           
002037         PERFORM S01-STARTA-DISPATCHEN                                    
010001                                                                          
020000       END-IF                                                             
030000       ADD +1 TO RAD-IX                                                   
040000     END-PERFORM                                                          
050000     MOVE RETT-1 (SPRAK-IX) TO MOD-MESSAGE-RAD23                          
060000                                                                          
061022     .                                                                    
070000     EJECT                                                                
071022 S01-STARTA-DISPATCHEN SECTION.                                           
072022                                                                          
073022     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
074023     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
075022     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
076022     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
077022     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
078022     MOVE 'W6I19B01'             TO MSG-KOM-IDCPYTXT                      
079022     MOVE 'INLEV   '             TO MSG-KOM-IDSNDNOD                      
079122     MOVE 'W6016100'             TO MSG-KOM-IDSNDJOB                      
079222     MOVE DAGENS-DATUM           TO MSG-KOM-TIREGDAT                      
079322     MOVE DAGENS-TID             TO MSG-KOM-TIKLOCK                       
079422     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
079522                                                                          
079622     MOVE ALL '+'                TO MOD619B-MID-W6I19B01                  
079731     MOVE WS-IDARTNR-NUM9        TO MOD619B-MID-IDARTNR                   
079932     MOVE WC-CDC-SE              TO MOD619B-MID-IDDC                      
080135     COMPUTE P-TO-P-KVLL       =  LNG-P-TO-P-PREFIX + 87                  
080231     MOVE 'W6T19BX '             TO P-TO-P-KDTRANS                        
080336     MOVE '9431'                 TO P-TO-P-IDTRANS                        
080431     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
080531     MOVE MOD619B-MID-W6I19B01   TO P-TO-P-DATA                           
080631                                                                          
080731     CALL W006KOM USING MSG-PCB                                           
080831                        DISP-PCB                                          
080931                        KOM-KOMA-PCB                                      
081031                        MSG-KOM-WMSGKOM                                   
081131                        P-TO-P-SW                                         
081231     .                                                                    
081331     EJECT                                                                
082000* MFS SECTIONER                                                           
090000     SKIP3                                                                
100000                                                                          
371021* IMS SECTIONER                                                           
380000 IMS-GET-MSG SECTION.                                                     
381021                                                                          
390000     MOVE '  QC' TO GODK-STATUSKODER                                      
400000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
410000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
420000     PERFORM IMS-STATUS-KONTROLL                                          
430000     SKIP3                                                                
450000     .                                                                    
460000 IMS-ISRT-MSG SECTION.                                                    
470000                                                                          
510000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
520000     MOVE SPACE TO GODK-STATUSKODER                                       
530000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
540000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
550000     PERFORM IMS-STATUS-KONTROLL                                          
560000     .                                                                    
570001     EJECT                                                                
700019 IMS-GU-ARTC        SECTION.                                              
710001     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
720003             DELIMITED BY SIZE    INTO SSA1                               
730020     MOVE 'WLARTC11 ' TO SSA2                                             
760001     MOVE '  GE' TO GODK-STATUSKODER                                      
770020     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1 SSA2                
780001     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
790001     PERFORM IMS-STATUS-KONTROLL                                          
800001     .                                                                    
810001     SKIP3                                                                
890001 IMS-REPLACE-ARTC SECTION.                                                
900001     MOVE '  ' TO GODK-STATUSKODER                                        
910001     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
920001     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
930001     PERFORM IMS-STATUS-KONTROLL                                          
940001     .                                                                    
950001     EJECT                                                                
960000 IMS-STATUS-KONTROLL SECTION.                                             
970000     SET STATUS-IX TO 1                                                   
980000     SEARCH GODK-STATUS AT END CALL FELLOG                                
990000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
000000     END-SEARCH                                                           
010000     CONTINUE                                                             
020000     .                                                                    
