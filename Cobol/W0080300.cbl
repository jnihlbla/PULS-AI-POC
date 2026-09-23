030000     SKIP3                                                                
040000 ID DIVISION.                                                             
050000     SKIP2                                                                
060000 PROGRAM-ID.     W0080300.                                                
100000*AUTHOR.         BO BERNTSSON.                                            
110000*DATE-WRITTEN.   DEC 83.                                                  
120000                                                                          
140000                                                                          
150000*    FUNKTION.                                                            
160000*       IMSDC FRÅGE OCH UPPDATERINGSPROGRAM                               
170000*       FÖR FÖRSÄKRANSTEXTER PÅ WDGX-4731                                 
180000                                                                          
190000*       ETT TEXTSEGMENT MOTSVARAR EN RAD PÅ BILDEN                        
200000                                                                          
210000*       MAXIMALT LÄSES 16 TEXTSEGMENT PER BILD . 15 RADER                 
220000*       VISAS OCH DET 16:E SEGMENTET INDIKERAR MER INFORMATION            
230000                                                                          
240000*       "SEGMENT-RÄKNARE" FINNS FÖR ATT HÅLLA                             
250000*       REDA PÅ ANTAL LÄSTA SEGMENT PER ROT OCH BILD                      
260000                                                                          
270000*       REGLER FÖR UPPDATERING:                                           
280000*       -LÄSNING SKER ALLTID FÖRST                                        
290000*       -ALL UPPDATERING SKER GENOM EN HJÄLP-ROT + BARN SOM UPP-          
300000*          DATERAS FÄRDIGT INNAN MAN UPPDATERAR DEN EGENTLIGA             
310000*          ROTEN. DETTA GÖRS FÖR ATT INGA TIDSGLAPP MELLAN                
320000*          DELETE OCH INSERT SKALL UPPSTÅ OCH STÖRA FAKTURERING.          
330000*       -OM ALLA RADER(SEGM.) UNDER EN NYCKEL(ROT) TAGES BORT,            
340000*          TAGES ROTEN BORT                                               
350000*       -NYUPPLÄGG AV ROT OCH SEGMENT ÄR MÖJLIGT.                         
360000                                                                          
370000                                                                          
380000                                                                          
390000*    INDATA.                                                              
400000*        TRANSAKTION: W0T803                                              
410000*                     W0T803U                                             
420000*                                                                         
430000*        MID:         W0I80301                                            
440000                                                                          
450000*    UTDATA.                                                              
460000*        MOD:         W0O80301                                            
470000     SKIP3                                                                
480000 ENVIRONMENT DIVISION.                                                    
490000     SKIP3                                                                
500000 DATA DIVISION.                                                           
510000     EJECT                                                                
520000 WORKING-STORAGE SECTION.                                                 
520102                                                                          
521002*    -- CHECKED BY WY2000                                                 
530001 77  IDPGM                       PIC X(8)    VALUE 'W0080300'.            
600000 77    ANVENT-JA                 PIC X.                                   
610000 77    SWE-JA                    PIC X       VALUE 'J'.                   
620000 77    ENG-JA                    PIC X       VALUE 'Y'.                   
630000 77    NEJ                       PIC X       VALUE 'N'.                   
640000 77    SLUT                      PIC X       VALUE 'S'.                   
650000 77    WS-KDFORSKN               PIC X(3).                                
660000 77    MAX-MOD-LAENGD            PIC S9(4)   VALUE +889 COMP SYNC.        
670000 77    WS-KVANTAL-TEXTRAD        PIC S9(6).                               
680000 77    WS-FLSTART-UPPDAT         PIC X(1).                                
690000 77    WS-IDTRANS                PIC X(4).                                
700000       88  WS-GODKAEND-BILD                  VALUE '0803'.                
710000*- - - - - - - - - - - - - - - - -INDEX RÄKNARE                           
720000 77    IX                        PIC S9(9)   VALUE +0 COMP SYNC.          
730000 77    IX-MAX                    PIC S9(9)   VALUE +16 COMP SYNC.         
740000 77    INDX                      PIC S9(9)   VALUE +0 COMP SYNC.          
750000*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -        
760000 01    PLUSTECKEN                PIC X(50)   VALUE ALL '+'.               
761001                                                                          
762001 01  DYNAMISKA-SUBPROGRAM.                                                
763001   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
764001   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
765001                                                                          
765002*    WS-BEFOSKN ANVÄNDS EJ, BORTTAGEN PGA COBOL-5 PROBLEM                 
770000*01    FILLER.                                                            
780000*  03    WS-BEFORSKN             PIC X(50)   OCCURS 15.                   
790000     EJECT                                                                
800000 01    NYCKLAR-TILL-DLI.                                                  
810000   03    W-KDFORSKN-4731-X.                                               
820000     05    IDHTYP                 PIC X(4)    VALUE '4731'.               
830000     05    W-KDFORSKN-4731        PIC S9(3)   COMP-3 VALUE ZERO.          
840000     05    FILLER                 PIC X(24)   VALUE LOW-VALUE.            
850000     SKIP3                                                                
860000   03    W-KDFORSKN-4701-X.                                               
870000     05    IDHTYP                 PIC X(4)    VALUE '4701'.               
880000     05    W-USERID-4701          PIC X(8).                               
890000     05    W-KDFORSKN-4701        PIC S9(3)   COMP-3 VALUE ZERO.          
900000     05    FILLER                 PIC X(16)   VALUE LOW-VALUE.            
910000     SKIP3                                                                
920000 01    MEDDELANDE.                                                        
930000   03    FEL-1                   PIC X(10).                               
940000   03    FEL-2                   PIC X(25).                               
950000   03    MED-1                   PIC X(36).                               
960000   03    MED-2                   PIC X(9).                                
970000   03    MED-3                   PIC X(25).                               
980000     EJECT                                                                
990000                                                                          
000000 01    SVENSKA-MEDDELANDE.                                                
010000   03    FILLER                  PIC X(10)   VALUE                        
020000             'FEL NYCKEL'.                                                
030000   03    FILLER                  PIC X(25)   VALUE                        
040000             'INMATNINGSFÄLT FEL IFYLLT'.                                 
050000   03    FILLER                  PIC X(36)   VALUE                        
060000             'FÖRSÄKRANSTEXT FINNS EJ I DATABASEN'.                       
070000   03    FILLER                  PIC X(9)   VALUE                         
080000             'MER FINNS'.                                                 
090000   03    FILLER                  PIC X(25)  VALUE                         
100000             'FÖRSÄKRANSTEXT UPPDATERAD'.                                 
110000     EJECT                                                                
120000 01    ENGELSKA-MEDDELANDE.                                               
130000   03    FILLER                  PIC X(10)   VALUE                        
140000             'WRONG KEY '.                                                
150000   03    FILLER                  PIC X(25)   VALUE                        
160000             'INPUT FIELD NOT CORRECT  '.                                 
170000   03    FILLER                  PIC X(36)   VALUE                        
180000             'CERTIFICATION IS NOT IN THE DATABASE'.                      
190000   03    FILLER                  PIC X(9)   VALUE                         
200000             'MORE DATA'.                                                 
210000   03    FILLER                  PIC X(25)  VALUE                         
220000             'CERTIFICATION UPDATED    '.                                 
230000     EJECT                                                                
240000******************************************************************        
250000*                                                                         
260000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
270000*                                                                         
280000 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
290000     SKIP3                                                                
300000*01    MID -COPY W0I80301 -PRE MID-.                                      
320000     EJECT                                                                
330000*01    -COPY WMSGAREA                                                     
350000     EJECT                                                                
360000*    03  MOD -COPY W0O80301 -PRE MOD- -RED MSG-AREA.                      
380000     EJECT                                                                
390000*01    -COPY WMFSAREA                                                     
410000     EJECT                                                                
420000******************************************************************        
430000*                                                                         
440000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
450000*                                                                         
460000 01    IMS-WS.                                                            
470000   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
480000     SKIP3                                                                
490000*                        **** STATUS-KOD FRÅN IMS                         
500000   03    STATUS-WS               PIC XX.                                  
510000     88    SEGMENT-FINNS                     VALUE '  '.                  
520000     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
530000     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
540000     SKIP3                                                                
550000   03    GODK-STATUSKODER.                                                
560000     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
570000     SKIP3                                                                
580000 01    SSA1                      PIC X(64).                               
590000 01    SSA2                      PIC X(64).                               
600000     EJECT                                                                
610000*                            IMS FUNKTIONSKODER                           
620000*01    -COPY W0003                                                        
640000     EJECT                                                                
650000*- - - - - - - - - - - - - - -DLI INPUT-OUTPUT AREA                       
660000 01  DLI-IO-AREA.                                                         
670000    03 IO-AREA                  PIC X(100)    VALUE SPACE.                
680000*   03 WLXXDB01   -COPY WDGX473A   -RED IO-AREA                           
700000*                                                                         
710000*   03 WLXXDB11   -COPY WDGX4731   -RED IO-AREA -PRE 4731-                
730000     EJECT                                                                
740000*   03 WLXXDA01   -COPY WDGX4701   -RED IO-AREA                           
760000*                                                                         
770000*   03 WLXXDA11   -COPY WDGX4731   -RED IO-AREA -PRE 4701-                
790000     EJECT                                                                
800000 LINKAGE SECTION.                                                         
810000*01    -COPY W0009     -PRE MSG-                                          
830000     EJECT                                                                
840000*01    -COPY W0008     -PRE DA-                                           
860000     05  FILLER                  PIC X.                                   
870000     SKIP3                                                                
880000*01    -COPY W0008     -PRE DB-                                           
900000     05  FILLER                  PIC X.                                   
910000     EJECT                                                                
920000 PROCEDURE DIVISION USING MSG-PCB DA-PCB DB-PCB.                          
930000     ENTRY 'DLITCBL' USING MSG-PCB DA-PCB DB-PCB                          
940000     PERFORM IMS-GET-MSG                                                  
950000     IF SEGMENT-FINNS                                                     
960000       PERFORM A-INIT-SPARA-KOLLA-INPUT                                   
970000       IF WS-GODKAEND-BILD                                                
980000         IF WS-KDFORSKN NUMERIC                                           
990000           MOVE WS-KDFORSKN TO W-KDFORSKN-4731 W-KDFORSKN-4701            
000000           IF MFS-UPDATE                                                  
010000             IF MID-KDUPPD-FORSTEXT-UT = ANVENT-JA                        
020000               MOVE MID-KDUPPD-FORSTEXT-UT TO                             
030000                                        MOD-KDUPPD-FORSTEXT-UT            
040000               IF MID-KDUPPD-FORSTEXT-IN = SLUT                           
050000                 PERFORM C-UPPDATERA-4731                                 
060000                 PERFORM MFS-FLYTTA-TILL-MOD                              
070000                 MOVE MFS-RENSA-FAELT TO MOD-KDUPPD-FORSTEXT-UT           
080000               ELSE                                                       
090000                 PERFORM B-UPPDATERA-4701                                 
100000                 PERFORM D-LAES-OCH-FLYTTA-TILL-MOD                       
110000               END-IF                                                     
120000             ELSE                                                         
130000               MOVE MFS-ROER-EJ-FAELT TO MOD-KDUPPD-FORSTEXT-UT           
140000               MOVE FEL-2 TO MOD-MESSAGE-RAD1                             
150000               PERFORM MFS-FLYTTA-TILL-MOD                                
160000             END-IF                                                       
170000           ELSE                                                           
180000             IF MID-KDUPPD-FORSTEXT-IN = ANVENT-JA                        
190000               MOVE ANVENT-JA TO MOD-KDUPPD-FORSTEXT-UT                   
200000               PERFORM D-LAES-OCH-FLYTTA-TILL-MOD                         
210000             ELSE                                                         
220000               PERFORM D-LAES-OCH-FLYTTA-TILL-MOD                         
230000               MOVE NEJ TO MOD-KDUPPD-FORSTEXT-UT                         
240000             END-IF                                                       
250000           END-IF                                                         
260000         ELSE                                                             
270000           MOVE FEL-1 TO MOD-MESSAGE-RAD1                                 
280000           PERFORM MFS-FLYTTA-TILL-MOD                                    
290000         END-IF                                                           
300000       ELSE                                                               
310000         MOVE MFS-RENSA-FAELT TO MOD-KDFORSKN-UT                          
320000                                 MOD-KDUPPD-FORSTEXT-UT                   
330000       END-IF                                                             
340000       MOVE WS-KVANTAL-TEXTRAD TO MOD-KVANTAL-TEXTRAD                     
350000       MOVE WS-FLSTART-UPPDAT TO MOD-FLSTART-UPPDAT                       
360000       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
370000       PERFORM IMS-INSERT-MSG                                             
380000     END-IF                                                               
390000     MOVE ZERO TO RETURN-CODE                                             
400000     GOBACK                                                               
410000     CONTINUE.                                                            
420000     EJECT                                                                
430000 A-INIT-SPARA-KOLLA-INPUT SECTION.                                        
440000     SKIP2                                                                
450000     IF MSG-DUBBLA-TRANSKODER                                             
460000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W0I80301                 
470000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
480000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
490000       IF MSG-IDTRANS-2 = '0803'                                          
500000         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
510000       ELSE                                                               
520000         MOVE SPACE TO MFS-KDTRTYP                                        
530000       END-IF                                                             
540000     ELSE                                                                 
550000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W0I80301                  
560000       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
570000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
580000     END-IF                                                               
590000     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
600000                                                                          
610000     IF SWEDISH-TEXT                                                      
620000       MOVE +1 TO INDX                                                    
630000       MOVE SVENSKA-MEDDELANDE TO MEDDELANDE                              
640000       MOVE SWE-JA TO ANVENT-JA                                           
650000     ELSE                                                                 
660000       MOVE +2 TO INDX                                                    
670000       MOVE ENGELSKA-MEDDELANDE TO MEDDELANDE                             
680000       MOVE ENG-JA TO ANVENT-JA                                           
690000     END-IF                                                               
700000     IF MID-KDFORSKN-IN = ALL '+'                                         
710000       IF MID-KDUPPD-FORSTEXT-IN = ANVENT-JA                              
720000         MOVE ZERO TO WS-KVANTAL-TEXTRAD                                  
730000         MOVE SWE-JA TO WS-FLSTART-UPPDAT                                 
740000         MOVE SPACE TO MFS-KDTRTYP                                        
750000       ELSE                                                               
760000         MOVE MID-KVANTAL-TEXTRAD TO WS-KVANTAL-TEXTRAD                   
770000         MOVE MID-FLSTART-UPPDAT TO WS-FLSTART-UPPDAT                     
780000       END-IF                                                             
790000       MOVE MID-KDFORSKN-UT TO WS-KDFORSKN                                
800000       INSPECT WS-KDFORSKN REPLACING ALL SPACE BY ZERO                    
810000     ELSE                                                                 
820000       MOVE MID-KDFORSKN-IN TO WS-KDFORSKN                                
830000       MOVE SPACE TO MFS-KDTRTYP                                          
840000       MOVE ZERO TO WS-KVANTAL-TEXTRAD                                    
850000       MOVE SWE-JA TO WS-FLSTART-UPPDAT                                   
860000     END-IF                                                               
870000     MOVE MSG-SIGNON-USERID TO W-USERID-4701                              
880000     MOVE LOW-VALUE TO MSG-AREA                                           
890000     MOVE 'W0O80301' TO MFS-IDMOD                                         
900000     MOVE '0803' TO MOD-IDTRANS                                           
910000     MOVE WS-KDFORSKN TO MOD-KDFORSKN-UT                                  
920000     INSPECT MOD-KDFORSKN-UT REPLACING LEADING ZERO BY SPACE              
930000     MOVE MFS-RENSA-FAELT TO MOD-KDFORSKN-IN                              
940000                             MOD-MESSAGE-RAD1                             
950000                             MOD-MESSAGE-RAD23                            
960000     CONTINUE.                                                            
970000     EJECT                                                                
980000 B-UPPDATERA-4701 SECTION.                                                
990000     SKIP2                                                                
000000     PERFORM IMS-GET-4701-ROT                                             
010000     IF SEGMENT-FINNS                                                     
020000       IF WS-FLSTART-UPPDAT = SWE-JA                                      
030000         PERFORM IMS-DELETE-4701-ROT                                      
040000         MOVE W-KDFORSKN-4701-X TO WLXXDA01                               
050000         PERFORM IMS-INSERT-4701-ROT                                      
060000       END-IF                                                             
070000     ELSE                                                                 
080000       MOVE W-KDFORSKN-4701-X TO WLXXDA01                                 
090000       PERFORM IMS-INSERT-4701-ROT                                        
100000     END-IF                                                               
110000     MOVE +1 TO IX                                                        
120000     PERFORM UNTIL                                                        
130000      NOT ( IX < IX-MAX )                                                 
140000       IF  MID-BEFORSKN (IX) = ALL '+'                                    
150000         CONTINUE                                                         
160000       ELSE                                                               
170000         MOVE '1' TO 4701-FOERS-KDSEGKEY                                  
180000         MOVE MID-BEFORSKN (IX) TO 4701-FOERS-BEFORSKN                    
190000         PERFORM IMS-INSERT-4701-BARN                                     
200000         MOVE NEJ TO WS-FLSTART-UPPDAT                                    
210000       END-IF                                                             
220000       ADD +1 TO IX                                                       
230000     END-PERFORM                                                          
240000     CONTINUE.                                                            
250000     EJECT                                                                
260000 C-UPPDATERA-4731 SECTION.                                                
270000     SKIP2                                                                
280000     PERFORM IMS-GET-4731-ROT                                             
290000     IF SEGMENT-FINNS                                                     
300000       PERFORM IMS-DELETE-4731-ROT                                        
310000     END-IF                                                               
320000     PERFORM IMS-GET-4701-ROT                                             
330000     IF WS-FLSTART-UPPDAT = NEJ                                           
340000     OR MID-BEFORSKN-GRUPP NOT = ALL '+'                                  
350000       MOVE W-KDFORSKN-4731-X TO WLXXDB01                                 
360000       PERFORM IMS-INSERT-4731-ROT                                        
370000       IF WS-FLSTART-UPPDAT = NEJ                                         
380000         PERFORM IMS-GET-4701-BARN                                        
390000         PERFORM UNTIL                                                    
400000          NOT ( SEGMENT-FINNS )                                           
410000           PERFORM IMS-INSERT-4731-BARN                                   
420000           PERFORM IMS-GET-4701-BARN                                      
430000         END-PERFORM                                                      
440000       END-IF                                                             
450000       IF MID-BEFORSKN-GRUPP NOT = ALL '+'                                
460000         MOVE +1 TO IX                                                    
470000         PERFORM UNTIL                                                    
480000          NOT ( IX < IX-MAX )                                             
490000           IF MID-BEFORSKN (IX) = ALL '+'                                 
500000             CONTINUE                                                     
510000           ELSE                                                           
520000             MOVE MID-BEFORSKN (IX) TO 4731-FOERS-BEFORSKN                
530000             PERFORM IMS-INSERT-4731-BARN                                 
540000           END-IF                                                         
550000           ADD +1 TO IX                                                   
560000         END-PERFORM                                                      
570000       END-IF                                                             
580000     END-IF                                                               
590000     PERFORM IMS-GET-4701-ROT                                             
600000     IF SEGMENT-FINNS                                                     
610000       PERFORM IMS-DELETE-4701-ROT                                        
620000     END-IF                                                               
630000     MOVE MED-3 TO MOD-MESSAGE-RAD1                                       
640000     MOVE ZERO TO WS-KVANTAL-TEXTRAD                                      
650000     PERFORM MFS-FLYTTA-TILL-MOD                                          
660000     CONTINUE.                                                            
670000     EJECT                                                                
680000 D-LAES-OCH-FLYTTA-TILL-MOD SECTION.                                      
690000     SKIP2                                                                
700000******************************************************************        
710000*    HÄR LÄSER MAN ROT + BARN.                                            
720000*    OM ALLA SEGMENT ÄR LÄSTA                                             
730000*    BÖRJAR MAN OM FRÅN FÖRSTA SEGMENTET.                                 
740000******************************************************************        
750000     PERFORM IMS-GET-4731-ROT                                             
760000     IF SEGMENT-FINNS                                                     
770000       PERFORM S03-AKT-4731-BARN                                          
780000       MOVE +1 TO IX                                                      
790000       PERFORM IMS-GET-4731-BARN                                          
800000       IF SEGMENT-SAKNAS AND MFS-KDTRTYP        = SPACE                   
810000         PERFORM IMS-GET-4731-ROT                                         
820000         PERFORM IMS-GET-4731-BARN                                        
830000         MOVE ZERO TO WS-KVANTAL-TEXTRAD                                  
840000       END-IF                                                             
850000       PERFORM UNTIL                                                      
860000        ( IX > IX-MAX )                                                   
870000         IF SEGMENT-FINNS                                                 
880000           IF IX = IX-MAX                                                 
890000             MOVE MED-2 TO MOD-MESSAGE-RAD1                               
900000           ELSE                                                           
910000             MOVE 4731-FOERS-BEFORSKN TO MOD-BEFORSKN (IX)                
920000             ADD +1 TO WS-KVANTAL-TEXTRAD                                 
930000           END-IF                                                         
940000           PERFORM IMS-GET-4731-BARN                                      
950000         ELSE                                                             
960000           IF IX = IX-MAX                                                 
970000             CONTINUE                                                     
980000           ELSE                                                           
990000             MOVE MFS-RENSA-FAELT TO MOD-BEFORSKN (IX)                    
000000           END-IF                                                         
010000         END-IF                                                           
020000         ADD +1 TO IX                                                     
030000       END-PERFORM                                                        
040000     ELSE                                                                 
050000       IF WS-FLSTART-UPPDAT = SWE-JA                                      
060000         MOVE MED-1 TO MOD-MESSAGE-RAD1                                   
070000         PERFORM MFS-FLYTTA-TILL-MOD                                      
080000       ELSE                                                               
090000         MOVE MFS-RENSA-FAELT TO MOD-BEFORSKN-GRUPP                       
100000       END-IF                                                             
110000     END-IF                                                               
120000     CONTINUE.                                                            
130000     EJECT                                                                
140000 S03-AKT-4731-BARN SECTION.                                               
150000***********************************************************               
160000* HÄR LÄSER MAN SIG FRAM SÅ MÅNGA SEGMENT SOM ANGES AV                    
170000* SEGMENTRÄKNAREN FÖR ATT FÅ RÄTT POSITION                                
180000***********************************************************               
190000                                                                          
200000     MOVE +0 TO IX                                                        
210000     PERFORM UNTIL                                                        
220000      NOT ( IX < WS-KVANTAL-TEXTRAD AND SEGMENT-FINNS )                   
230000       PERFORM IMS-GET-4731-BARN                                          
240000       ADD +1 TO IX                                                       
250000     END-PERFORM                                                          
260000     CONTINUE.                                                            
270000     EJECT                                                                
280000 MFS-FLYTTA-TILL-MOD SECTION.                                             
290000     SKIP2                                                                
300000     MOVE +1 TO IX                                                        
310000     PERFORM UNTIL                                                        
320000      NOT ( IX < IX-MAX )                                                 
330000       MOVE MFS-ROER-EJ-FAELT TO MOD-BEFORSKN (IX)                        
340000       ADD +1 TO IX                                                       
350000     END-PERFORM                                                          
360000     MOVE MFS-RENSA-FAELT TO MID-KDUPPD-FORSTEXT-IN                       
370000     CONTINUE.                                                            
380000     EJECT                                                                
390000* IMS SEKTIONER                                                           
400000     SKIP3                                                                
410000 IMS-GET-MSG SECTION.                                                     
420000     MOVE '  QC' TO GODK-STATUSKODER                                      
430001     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
440000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
450000     PERFORM IMS-STATUSKONTROLL                                           
460000     CONTINUE.                                                            
470000     SKIP3                                                                
480000     EJECT                                                                
490000 IMS-INSERT-MSG SECTION.                                                  
500000     IF ENGLISH-TEXT                                                      
510000       MOVE 'N' TO MFS-KDHUVOMR                                           
520000     END-IF                                                               
530000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
540000     MOVE SPACE TO GODK-STATUSKODER                                       
550001     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
560000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
570000     PERFORM IMS-STATUSKONTROLL                                           
580000     CONTINUE.                                                            
590000     EJECT                                                                
600000 IMS-GET-4731-ROT SECTION.                                                
610000     STRING 'WLXXDB01(WDGXKEY  =' W-KDFORSKN-4731-X ')'                   
620000            DELIMITED BY SIZE INTO SSA1                                   
630000     MOVE '  GE' TO GODK-STATUSKODER                                      
640001     CALL CBLTDLI USING GHU DB-PCB DLI-IO-AREA SSA1                       
650000     MOVE DB-STATUS-CODE TO STATUS-WS                                     
660000     PERFORM IMS-STATUSKONTROLL                                           
670000     CONTINUE.                                                            
680000     EJECT                                                                
690000 IMS-DELETE-4731-ROT SECTION.                                             
700000     MOVE '  ' TO GODK-STATUSKODER                                        
710001     CALL CBLTDLI USING DLET DB-PCB DLI-IO-AREA                           
720000     MOVE DB-STATUS-CODE TO STATUS-WS                                     
730000     PERFORM IMS-STATUSKONTROLL                                           
740000     CONTINUE.                                                            
750000     EJECT                                                                
760000 IMS-INSERT-4731-ROT SECTION.                                             
770000     MOVE 'WLXXDB01' TO SSA1                                              
780000     MOVE '  ' TO GODK-STATUSKODER                                        
790001     CALL CBLTDLI USING ISRT DB-PCB DLI-IO-AREA SSA1                      
800000     MOVE DB-STATUS-CODE TO STATUS-WS                                     
810000     PERFORM IMS-STATUSKONTROLL                                           
820000     CONTINUE.                                                            
830000     EJECT                                                                
840000 IMS-GET-4731-BARN SECTION.                                               
850000     MOVE 'WLXXDB11' TO SSA1                                              
860000     MOVE '  GE' TO GODK-STATUSKODER                                      
870001     CALL CBLTDLI USING GHNP DB-PCB DLI-IO-AREA SSA1                      
880000     MOVE DB-STATUS-CODE TO STATUS-WS                                     
890000     PERFORM IMS-STATUSKONTROLL                                           
900000     CONTINUE.                                                            
910000     EJECT                                                                
920000 IMS-INSERT-4731-BARN SECTION.                                            
930000     STRING 'WLXXDB01(WDGXKEY  ='  W-KDFORSKN-4731-X ')'                  
940000            DELIMITED BY SIZE INTO SSA1                                   
950000     MOVE 'WLXXDB11*L' TO SSA2                                            
960000     MOVE '  ' TO GODK-STATUSKODER                                        
970001     CALL CBLTDLI USING ISRT DB-PCB DLI-IO-AREA SSA1 SSA2                 
980000     MOVE DB-STATUS-CODE TO STATUS-WS                                     
990000     PERFORM IMS-STATUSKONTROLL                                           
000000     CONTINUE.                                                            
010000     EJECT                                                                
020000 IMS-GET-4701-ROT SECTION.                                                
030000     STRING 'WLXXDA01(WDGXKEY  =' W-KDFORSKN-4701-X ')'                   
040000            DELIMITED BY SIZE INTO SSA1                                   
050000     MOVE '  GE' TO GODK-STATUSKODER                                      
060001     CALL CBLTDLI USING GHU DA-PCB DLI-IO-AREA SSA1                       
070000     MOVE DA-STATUS-CODE TO STATUS-WS                                     
080000     PERFORM IMS-STATUSKONTROLL                                           
090000     CONTINUE.                                                            
100000     EJECT                                                                
110000 IMS-DELETE-4701-ROT SECTION.                                             
120000     MOVE '  ' TO GODK-STATUSKODER                                        
130001     CALL CBLTDLI USING DLET DA-PCB DLI-IO-AREA                           
140000     MOVE DA-STATUS-CODE TO STATUS-WS                                     
150000     PERFORM IMS-STATUSKONTROLL                                           
160000     CONTINUE.                                                            
170000     EJECT                                                                
180000 IMS-INSERT-4701-ROT SECTION.                                             
190000     MOVE 'WLXXDA01' TO SSA1                                              
200000     MOVE '  ' TO GODK-STATUSKODER                                        
210001     CALL CBLTDLI USING ISRT DA-PCB DLI-IO-AREA SSA1                      
220000     MOVE DA-STATUS-CODE TO STATUS-WS                                     
230000     PERFORM IMS-STATUSKONTROLL                                           
240000     CONTINUE.                                                            
250000     EJECT                                                                
260000 IMS-GET-4701-BARN SECTION.                                               
270000     MOVE 'WLXXDA11' TO SSA1                                              
280000     MOVE '  GE' TO GODK-STATUSKODER                                      
290001     CALL CBLTDLI USING GHNP DA-PCB DLI-IO-AREA SSA1                      
300000     MOVE DA-STATUS-CODE TO STATUS-WS                                     
310000     PERFORM IMS-STATUSKONTROLL                                           
320000     CONTINUE.                                                            
330000     EJECT                                                                
340000 IMS-INSERT-4701-BARN SECTION.                                            
350000     STRING 'WLXXDA01(WDGXKEY  ='  W-KDFORSKN-4701-X ')'                  
360000            DELIMITED BY SIZE INTO SSA1                                   
370000     MOVE 'WLXXDA11*L' TO SSA2                                            
380000     MOVE '  ' TO GODK-STATUSKODER                                        
390001     CALL CBLTDLI USING ISRT DA-PCB DLI-IO-AREA SSA1 SSA2                 
400000     MOVE DA-STATUS-CODE TO STATUS-WS                                     
410000     PERFORM IMS-STATUSKONTROLL                                           
420000     CONTINUE.                                                            
430000     EJECT                                                                
440000 IMS-STATUSKONTROLL SECTION.                                              
450000     SET STATUS-IX TO 1                                                   
460001     SEARCH GODK-STATUS                                                   
461001       AT END                                                             
462001         CALL FELLOG                                                      
470000     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
480000     END-SEARCH                                                           
490000     CONTINUE                                                             
500000            CONTINUE.                                                     
510000 IMS-STATUSKONTROLL-EXIT. EXIT.                                           
520000     CONTINUE.                                                            
