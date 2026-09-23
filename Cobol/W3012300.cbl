000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3012300.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   DECEMBER 1999.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMETS HUVUDFUNKTIONER ÄR FÖLJANDE:                         
000900*        -FRÅGA PÅ POÄNGJUSTERINGAR,                                      
001000*         MATA IN DISTRIKT OCH EV. KONTO FÖR ATT FÅ INFORMATION           
001100*         OM VILKA OBEARBETADE JUSTERINGAR SOM FINNS FÖR AKTUELLT         
001200*         DISTRIKT/KONTO                                                  
001300*                                                                         
001400*        -LÄGGA UPP NYA JUSTERINGAR      (N)                              
001500*                                                                         
001600*        -ÄNDRA BEFINTLIGA JUSTERINGAR   (C)                              
001700*                                                                         
001800*        -TA BORT BEFINTLIGA JUSTERINGAR (D)                              
001900*                                                                         
002000*        PROGRAMMET LÄSER            WDB2                                 
002100*        PROGRAMMET LÄSER            WDA7 VIA WDA7ASEQ                    
002200*        PROGRAMMET UPPDATERAR       WDA7                                 
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W3T123                                              
002600*                     W3T123U                                             
002700*        MID:         W3I12301                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W3O12301                                            
003100     EJECT                                                                
003200                                                                          
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08)   VALUE 'W3012300'.            
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600                                                                          
004700*    --- GENERELLA ARBETSFÄLT                                             
004800 01  DAGENS-DATUM                PIC 9(8).                                
004900 01  DAGENS-TID                  PIC 9(8).                                
005000 01  WS-KDEXCHA                  PIC X(3).                                
494299 01  WS-KVPOINT                  PIC S9(7).                               
494399 01  WS-KDEXCHA-ALFA             PIC X(3).                                
494599 01  WS-RAD-ALFA                 PIC X(5).                                
494699 01  WS-KVPOINT-ALFA             PIC X(7).                                
500999                                                                          
501499*    --- INDEX FÖR BLÄDDRINGSRADER                                        
501599 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
502099 77  MAX-INDX                    PIC S9(4)   VALUE +12  COMP SYNC.        
510000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
530000                                                                          
540000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
540100     88  ALLT-OK                             VALUE 'J'.                   
540200                                                                          
540300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
540400     88  INDATA-OK                           VALUE 'J'.                   
541000     88  INDATA-FEL                          VALUE 'N'.                   
550000                                                                          
551099 77  TEXT-SW                     PIC X       VALUE 'N'.                   
552099     88  TEXT-KLAR                           VALUE 'J'.                   
554099                                                                          
560000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
570000     88  NYCKLAR-OK                          VALUE 'J'.                   
580000     88  NYCKLAR-FEL                         VALUE 'N'.                   
590000                                                                          
600000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
610099     88  EGEN-MID                            VALUE '3123'.                
620099     88  GODK-MID                            VALUE '3123'.                
670000     88  HELP-MID                            VALUE '0551'.                
680000     EJECT                                                                
681099                                                                          
690000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
700000 01  GENERELLA-SUBPROGRAM.                                                
710000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
720000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
730000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
740000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
760000     EJECT                                                                
761099                                                                          
770000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
780000*01 -COPY WMEDAREA                                                        
780199     EJECT                                                                
790099                                                                          
800000 01  MESSAGE-CODES.                                                       
810100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
810300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
810699     03  ERR-ITEM-MISSING        PIC X(3)    VALUE '029'.                 
810799     03  ERR-DIST-MISSING        PIC X(3)    VALUE '040'.                 
810899     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
810999     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
820100     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
820299     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
821000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
850000     EJECT                                                                
851099                                                                          
860000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
870000*                                                                         
880000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
890099                                                                          
900000*01 -COPY WMSGINIT                                                        
910100     EJECT                                                                
910299                                                                          
910335 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
910500*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
910600*                                                                         
910720 01  SPAR-AREA.                                                           
910899     03  SPAR-IDTRANS            PIC X(4)    VALUE '3123'.                
910999     03  SPAR-IDDISTR-ENTER      PIC 9(4)    VALUE ZERO.                  
911099     03  SPAR-KDEXCHA-ENTER      PIC 9(3)    VALUE ZERO.                  
911199     03  SPAR-IDBYTRAD-ENTER     PIC 9(5)    VALUE ZERO.                  
911299     03  SPAR-IDDISTR-NEXT       PIC 9(4)    VALUE ZERO.                  
912099     03  SPAR-KDEXCHA-NEXT       PIC 9(3)    VALUE ZERO.                  
913099     03  SPAR-IDBYTRAD-NEXT      PIC 9(5)    VALUE ZERO.                  
920000     EJECT                                                                
921099                                                                          
930000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
940000*                                                                         
950000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
960099                                                                          
970099*01  MID -COPY W3I12301                                                   
980000     EJECT                                                                
990099 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
000099                                                                          
010000*01  -COPY WMSGAREA                                                       
020000     EJECT                                                                
021099                                                                          
030000     03  MOD REDEFINES MSG-AREA.                                          
040099*      05  -COPY W3O12301                                                 
050000     EJECT                                                                
051099                                                                          
060000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
070099                                                                          
080000*01  -COPY WMFSAREA                                                       
090000     EJECT                                                                
091099                                                                          
100000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
110000*                                                                         
130000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
140099                                                                          
150000 01  NYCKLAR-TILL-DLI.                                                    
160100*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
160499     03  W-WDA701KY-X.                                                    
160599         05  W-IDPTYP        PIC X(3)    VALUE 'ADJ'.                     
160699         05  W-IDDISTR       PIC S9(5)   VALUE ZERO   COMP-3.             
160899         05  W-IDARTNR       PIC S9(9)   VALUE ZERO   COMP-3.             
160999         05  W-IDKUNDNR      PIC S9(7)   VALUE ZERO   COMP-3.             
161099         05  W-IDBYTRAD      PIC S9(5)   VALUE ZERO   COMP-3.             
161199     03  W-WDA7ASEQ-X.                                                    
161299         05  W-IDPTYP-A      PIC X(3)    VALUE 'ADJ'.                     
161399         05  W-IDDISTR-A     PIC S9(5)   VALUE ZERO   COMP-3.             
161499         05  W-KDEXCHA-A     PIC S9(3)   VALUE ZERO   COMP-3.             
161599         05  W-IDBYTRAD-A    PIC S9(5)   VALUE ZERO   COMP-3.             
161699     03  W-WDA7ASEQ-MIN-X.                                                
161799         05  W-IDPTYP-AMIN   PIC X(3)    VALUE 'ADJ'.                     
161899         05  W-IDDISTR-AMIN  PIC S9(5)   VALUE ZERO   COMP-3.             
161999         05  W-KDEXCHA-AMIN  PIC S9(3)   VALUE ZERO   COMP-3.             
162099         05  W-IDBYTRAD-AMIN PIC S9(5)   VALUE ZERO   COMP-3.             
162199     03  W-WDA7ASEQ-MAX-X.                                                
162299         05  W-IDPTYP-AMAX   PIC X(3)    VALUE 'ADJ'.                     
162399         05  W-IDDISTR-AMAX  PIC S9(5)   VALUE ZERO   COMP-3.             
162499         05  W-KDEXCHA-AMAX  PIC S9(3)   VALUE +999   COMP-3.             
162599         05  W-IDBYTRAD-AMAX PIC S9(5)   VALUE +99999 COMP-3.             
163099     03  W-WDB201KEY-MIN-X.                                               
164099         05  W-IDDISTR-WDB-MIN  PIC S9(5) VALUE ZERO   COMP-3.            
165099         05  W-IDKUNDNR-WDB-MIN PIC S9(7) VALUE ZERO   COMP-3.            
166099     03  W-WDB201KEY-MAX-X.                                               
167099         05  W-IDDISTR-WDB-MAX  PIC S9(5) VALUE ZERO    COMP-3.           
168099         05  W-IDKUNDNR-WDB-MAX PIC S9(7) VALUE 9999999 COMP-3.           
170099                                                                          
180000*    --- STATUS-KOD FRÅN IMS                                              
190000 01  STATUS-WS                   PIC XX.                                  
200000     88  SEGMENT-FINNS                       VALUE '  '.                  
210000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
211099     88  INDEX-FINNS-REDAN                   VALUE 'NI'.                  
220000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
221051     88  BAS-SLUT                            VALUE 'GB'.                  
230099                                                                          
240000 01  GODK-STATUSKODER.                                                    
250000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
260099                                                                          
270099 01  SSA1                        PIC X(128).                              
280099 01  SSA2                        PIC X(128).                              
281099 01  SSA3                        PIC X(64).                               
290000     EJECT                                                                
291099                                                                          
300000*    --- IMS FUNKTIONSKODER                                               
310000*01  -COPY W0003                                                          
330000     EJECT                                                                
331099                                                                          
340000*    ---  DLI INPUT-OUTPUT AREA                                           
350000                                                                          
360199 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA701  '.                    
360299 01  DLI-IO-WDA701.                                                       
360399*    03  -COPY WDA701                                                     
360400     EJECT                                                                
360599                                                                          
360699 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201  '.                    
360799 01  DLI-IO-WDB201.                                                       
361099*    03  -COPY WDB201                                                     
390000     EJECT                                                                
399399                                                                          
400000 LINKAGE SECTION.                                                         
410099*01  -COPY W0009  -PRE MSG-                                               
420099*01  -COPY W0008  -PRE USEA-                                              
430000     05  FILLER                  PIC X.                                   
440100                                                                          
440299*01  -COPY W0008  -PRE WDA7A-                                             
441000     05  FILLER                  PIC X.                                   
441199*01  -COPY W0008  -PRE WDA7-                                              
441299     05  FILLER                  PIC X.                                   
441399*01  -COPY W0008  -PRE WDB2-                                              
441499     05  FILLER                  PIC X.                                   
450000     EJECT                                                                
460099                                                                          
460199 PROCEDURE DIVISION  USING MSG-PCB   USEA-PCB                             
460299                           WDA7A-PCB WDA7-PCB WDB2-PCB.                   
460399                                                                          
460499 MAIN SECTION.                                                            
461099     ENTRY 'DLITCBL' USING MSG-PCB   USEA-PCB                             
461199                           WDA7A-PCB WDA7-PCB WDB2-PCB.                   
462099                                                                          
490000     PERFORM IMS-GET-MSG                                                  
500000     IF SEGMENT-FINNS                                                     
510000       PERFORM A-INIT                                                     
520099       PERFORM B-KONTROLLERA-NYCKLAR                                      
530000       IF NYCKLAR-OK                                                      
540100         IF MFS-UPDATE                                                    
540299           PERFORM G-KONTROLLERA-INPUT                                    
540300           IF INDATA-OK                                                   
540400             PERFORM H-UPPDATERA                                          
540500           END-IF                                                         
541000         ELSE                                                             
550100           IF MFS-FIRST                                                   
550200             PERFORM C-FOERSTA-SIDA                                       
550300           ELSE                                                           
550400             IF MFS-NEXT                                                  
550500               PERFORM D-NAESTA-SIDA                                      
550600             ELSE                                                         
550700               PERFORM E-SAMMA-SIDA                                       
550800             END-IF                                                       
551000           END-IF                                                         
571000         END-IF                                                           
572099         IF ALLT-OK                                                       
572199           PERFORM F-LAES-VISA                                            
573099         END-IF                                                           
590000       END-IF                                                             
620099       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O12301 + 4                      
630000       PERFORM IMS-INSERT-MSG                                             
640000     END-IF                                                               
660000                                                                          
670000     MOVE ZERO TO RETURN-CODE                                             
680000     GOBACK                                                               
690000     .                                                                    
700000     EJECT                                                                
701099                                                                          
710000 A-INIT SECTION.                                                          
721099     MOVE FUNCTION CURRENT-DATE(1:8)      TO   DAGENS-DATUM               
721299     ACCEPT DAGENS-TID                    FROM TIME                       
722000                                                                          
730000     IF MSG-DUBBLA-TRANSKODER                                             
740099       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I12301                 
750099       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
760099       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
770000     ELSE                                                                 
780099       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I12301                 
790099       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
800099       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
810000     END-IF                                                               
820000                                                                          
830099     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
840099     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
850099     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
860000                                                                          
870099     MOVE LOW-VALUE                       TO MSG-AREA                     
880099     MOVE 'W3O123N1'                      TO MFS-IDMOD                    
890099     MOVE '3123'                          TO MOD-IDTRANS                  
900099     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
900199                                             MOD-TEMFSINF                 
900299                                                                          
920000     IF EGEN-MID OR HELP-MID                                              
930000       CONTINUE                                                           
940000     ELSE                                                                 
950099       MOVE SPACE                         TO MFS-KDTRTYP                  
960099       MOVE '7'                           TO MFS-IDPFK                    
970000     END-IF                                                               
971099                                                                          
980099     MOVE 'GB'                            TO MED-IDSKYLT                  
000000     .                                                                    
010000     EJECT                                                                
011099                                                                          
020099 B-KONTROLLERA-NYCKLAR SECTION.                                           
040099     MOVE ALL '+'             TO MSGI-WMSGINIT                            
050099     MOVE '001'               TO MSGI-KDCALL                              
060099     MOVE MSG-LTERM-NAME      TO MSGI-IDLTERM-USER                        
070099     MOVE MSG-SIGNON-USERID   TO MSGI-IDUSER                              
080099     MOVE '3123'              TO MSGI-IDTRANS                             
081000                                                                          
090000     IF EGEN-MID                                                          
091099       IF MID-IDDISTR-IN NOT = ALL '+'                                    
100099         MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                             
230099       END-IF                                                             
230199     END-IF                                                               
231099                                                                          
240000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
250099     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
250197                                                                          
252099     MOVE JA                  TO ALLT-SW                                  
253099                                 NYCKLAR-SW                               
257000                                                                          
258099*    -- KONTROLL AV DISTRIKT                                              
259099     MOVE MFS-RENSA-FAELT     TO MOD-IDDISTR-IN                           
259100                                                                          
259399     IF MSGI-IDDISTR NOT = ALL '+'                                        
259499       INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO               
259599       IF MSGI-IDDISTR NUMERIC                                            
259699         MOVE MSGI-IDDISTR    TO W-IDDISTR                                
259799                                 W-IDDISTR-A                              
259899                                 W-IDDISTR-AMIN                           
259999                                 W-IDDISTR-AMAX                           
260199       ELSE                                                               
261099         MOVE NEJ             TO NYCKLAR-SW                               
262099                                 ALLT-SW                                  
263099       END-IF                                                             
263199     ELSE                                                                 
264099       MOVE NEJ               TO NYCKLAR-SW                               
265099                                 ALLT-SW                                  
270077     END-IF                                                               
272400                                                                          
272599*    -- KONTROLL AV KONTO                                                 
272699     PERFORM BB-KONTROLL-KDEXCHA                                          
272799                                                                          
272899     IF GODK-MID OR NYCKLAR-OK                                            
272999       MOVE MSGI-IDDISTR      TO MOD-IDDISTR-UT                           
273099       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
273199       MOVE WS-KDEXCHA        TO MOD-KDEXCHA-UT                           
273299       INSPECT MOD-KDEXCHA-UT REPLACING LEADING ZERO BY SPACE             
273399     ELSE                                                                 
273499       MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-UT                           
273599                                 MOD-KDEXCHA-UT                           
273700     END-IF                                                               
273899                                                                          
273999     IF MID-IDDISTR-IN NOT = ALL '+' OR                                   
274099        MID-KDEXCHA-IN NOT = ALL '+'                                      
274199       MOVE SPACE             TO MFS-KDTRTYP                              
274299       MOVE '7'               TO MFS-IDPFK                                
274399     END-IF                                                               
274499                                                                          
274599     IF NYCKLAR-FEL                                                       
274699       MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                             
274799       CALL WMEDKONV USING MED-WMEDAREA                                   
274899       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
274999       PERFORM MFS-RENSA-FAELT-IN                                         
275099       PERFORM MFS-RENSA-FAELT-UT                                         
275199     END-IF                                                               
275299     .                                                                    
275399     EJECT                                                                
275499                                                                          
275599 BB-KONTROLL-KDEXCHA SECTION.                                             
275699     IF MID-KDEXCHA-IN = ALL '+'                                          
275799       IF MFS-FIRST OR MFS-NEXT OR MFS-UPDATE                             
275899         MOVE MID-KDEXCHA-UT      TO WS-KDEXCHA                           
276199         INSPECT WS-KDEXCHA REPLACING LEADING SPACE BY ZERO               
276200         IF WS-KDEXCHA NOT NUMERIC                                        
276210            MOVE ZERO TO WS-KDEXCHA                                       
276220         END-IF                                                           
276299         MOVE WS-KDEXCHA          TO W-KDEXCHA-A                          
276499         IF WS-KDEXCHA = '000'                                            
276599           MOVE ZERO              TO W-KDEXCHA-AMIN                       
276699           MOVE 99999             TO W-KDEXCHA-AMAX                       
276799         ELSE                                                             
276899           MOVE WS-KDEXCHA        TO W-KDEXCHA-AMIN                       
276999           MOVE 99999             TO W-KDEXCHA-AMAX                       
277099         END-IF                                                           
277199       ELSE                                                               
277299         MOVE '000'               TO WS-KDEXCHA                           
277399         MOVE ZERO                TO W-KDEXCHA-A                          
277599                                     W-KDEXCHA-AMIN                       
277699         MOVE 99999               TO W-KDEXCHA-AMAX                       
277799       END-IF                                                             
277899     ELSE                                                                 
277999       MOVE MID-KDEXCHA-IN        TO WS-KDEXCHA                           
278099       INSPECT WS-KDEXCHA REPLACING LEADING SPACE BY ZERO                 
278199       IF WS-KDEXCHA NOT NUMERIC                                          
278299         MOVE NEJ                 TO NYCKLAR-SW                           
278399                                     ALLT-SW                              
278499       ELSE                                                               
278599         MOVE WS-KDEXCHA          TO W-KDEXCHA-A                          
278799                                     W-KDEXCHA-AMIN                       
278899         MOVE 99999               TO W-KDEXCHA-AMAX                       
278999       END-IF                                                             
279099     END-IF                                                               
281299     .                                                                    
281399     EJECT                                                                
281499                                                                          
281599 C-FOERSTA-SIDA SECTION.                                                  
281699     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
281799     CALL WMEDKONV USING MED-WMEDAREA                                     
281899     MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                  
281999                                                                          
282099     PERFORM MFS-RENSA-FAELT-IN                                           
282199     .                                                                    
282299     EJECT                                                                
282399                                                                          
282499 D-NAESTA-SIDA SECTION.                                                   
282599     IF SPAR-IDTRANS = '3123'                                             
282699       MOVE SPAR-IDDISTR-NEXT  TO W-IDDISTR                               
282799                                  W-IDDISTR-A                             
282999       MOVE SPAR-KDEXCHA-NEXT  TO W-KDEXCHA-A                             
283199       MOVE SPAR-IDBYTRAD-NEXT TO W-IDBYTRAD                              
283299                                  W-IDBYTRAD-A                            
283399     ELSE                                                                 
283499       PERFORM MFS-RENSA-FAELT-IN                                         
283599     END-IF                                                               
283699     .                                                                    
283799     EJECT                                                                
283899                                                                          
283999 E-SAMMA-SIDA SECTION.                                                    
284099     IF SPAR-IDTRANS = '3123' OR                                          
284199                       '0551'                                             
284299       MOVE SPAR-IDDISTR-ENTER  TO W-IDDISTR                              
284399                                   W-IDDISTR-A                            
284499       MOVE SPAR-KDEXCHA-ENTER  TO W-KDEXCHA-A                            
284699       MOVE SPAR-IDBYTRAD-ENTER TO W-IDBYTRAD                             
284799                                   W-IDBYTRAD-A                           
284899       MOVE INF-PRESS-PF11      TO MED-IDMFSFEL                           
284999       CALL WMEDKONV USING MED-WMEDAREA                                   
285099       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
285199       PERFORM MFS-ROER-EJ-FAELT-IN                                       
285299       PERFORM MFS-LAES-IN-IGEN                                           
285399     ELSE                                                                 
285499       PERFORM MFS-RENSA-FAELT-IN                                         
285599     END-IF                                                               
286099     .                                                                    
290029     EJECT                                                                
300099                                                                          
431099 F-LAES-VISA SECTION.                                                     
431199     IF MFS-NEXT OR MFS-UPDATE                                            
431599       PERFORM IMS-GU-WDA701-ASEQ                                         
431899     ELSE                                                                 
433199       PERFORM IMS-GN-WDA701-ASEQ                                         
433299       IF SEGMENT-SAKNAS OR                                               
433399          BAS-SLUT                                                        
433499         MOVE ERR-ITEM-MISSING   TO MED-IDMFSFEL                          
433599         CALL WMEDKONV USING MED-WMEDAREA                                 
433699         MOVE MED-MFSFEL         TO MOD-TEMFSFEL                          
434099       END-IF                                                             
435099     END-IF                                                               
440099     IF SEGMENT-FINNS                                                     
440199       MOVE ROT-IDDISTR          TO SPAR-IDDISTR-ENTER                    
441099       MOVE ROT-KDEXCHA          TO SPAR-KDEXCHA-ENTER                    
442099       MOVE ROT-IDBYTRAD         TO SPAR-IDBYTRAD-ENTER                   
443099     END-IF                                                               
450099                                                                          
451025     MOVE +1 TO INDX                                                      
452099     PERFORM UNTIL INDX > MAX-INDX OR                                     
452199                   SEGMENT-SAKNAS  OR                                     
452299                   BAS-SLUT                                               
453025       IF SEGMENT-FINNS                                                   
454099         MOVE ROT-IDDISTR        TO MOD-IDDISTR (INDX)                    
455699         MOVE ROT-KDEXCHA        TO MOD-KDEXCHA (INDX)                    
455799         MOVE ROT-IDBYTRAD       TO MOD-IDBYTRAD (INDX)                   
455899         MOVE ROT-TENOTE         TO MOD-TENOTE (INDX)                     
455999         MOVE ROT-KVPOINT        TO MOD-KVPOINT (INDX)                    
456099         MOVE ROT-IDUSER         TO MOD-IDUSER (INDX)                     
456199         MOVE ROT-DAREGDAT       TO MOD-DAREGDAT (INDX)                   
456299       ELSE                                                               
457025         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
458025       END-IF                                                             
459025       ADD +1 TO INDX                                                     
459199       PERFORM IMS-GN-WDA701-ASEQ                                         
459599     END-PERFORM                                                          
459699                                                                          
459799     IF SEGMENT-FINNS                                                     
459899       MOVE ROT-IDDISTR            TO SPAR-IDDISTR-NEXT                   
459999       MOVE ROT-KDEXCHA            TO SPAR-KDEXCHA-NEXT                   
460099       MOVE ROT-IDBYTRAD           TO SPAR-IDBYTRAD-NEXT                  
460299       IF NOT MFS-UPDATE                                                  
460399         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
460499         CALL WMEDKONV USING MED-WMEDAREA                                 
460599         MOVE MED-TEMFSINF         TO MOD-TEMFSINF                        
460699       END-IF                                                             
460799     END-IF                                                               
460899                                                                          
460999     MOVE '002'          TO MSGI-KDCALL                                   
461099     MOVE '3123'         TO SPAR-IDTRANS                                  
461199     MOVE SPAR-AREA      TO MSGI-SPAR-AREA                                
461299     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
462099     .                                                                    
470025     EJECT                                                                
480099                                                                          
520299 G-KONTROLLERA-INPUT SECTION.                                             
520499     MOVE JA                        TO INDATA-SW                          
520599                                       ALLT-SW                            
520699     MOVE NEJ                       TO TEXT-SW                            
520799                                                                          
520899     IF MID-CMD          = ALL '+' AND                                    
520999        MID-IDDISTR-UPD  = ALL '+' AND                                    
521099        MID-KDEXCHA-UPD  = ALL '+' AND                                    
521299        MID-IDBYTRAD-UPD = ALL '+' AND                                    
521399        MID-TENOTE-UPD   = ALL '+' AND                                    
521499        MID-KVPOINT-UPD  = ALL '+' AND                                    
521599        MID-FLDEBCRE-UPD = ALL '+'                                        
521699       MOVE ERR-PF11-AND-NO-DATA    TO MED-IDMFSFEL                       
521799       CALL WMEDKONV USING MED-WMEDAREA                                   
521899       MOVE MED-MFSFEL              TO MOD-TEMFSFEL                       
521999       PERFORM MFS-ROER-EJ-FAELT-IN                                       
522099       PERFORM MFS-ROER-EJ-FAELT-UT                                       
522199       MOVE NEJ                     TO INDATA-SW                          
522299                                       ALLT-SW                            
522399     ELSE                                                                 
522499                                                                          
522599*   KONTROLLERA KOMMANDO                                                  
522699       IF MID-CMD = 'N' OR                                                
522799          MID-CMD = 'C' OR                                                
522899          MID-CMD = 'D'                                                   
522999         MOVE MID-CMD               TO MOD-CMD                            
523099         MOVE MFS-ALFA-FAELT-RAETT  TO MOD-CMD-ATTR                       
523199       ELSE                                                               
523299         MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                       
523399         MOVE MFS-ALFA-FAELT-FEL    TO MOD-CMD-ATTR                       
523499         MOVE NEJ                   TO INDATA-SW                          
523599                                       ALLT-SW                            
523699       END-IF                                                             
523799                                                                          
523899*   KONTROLLERA DISTRIKT                                                  
526499       IF MID-IDDISTR-UPD = ALL '+'                                       
526599         MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                       
526899         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-UPD-ATTR               
526999         MOVE NEJ                   TO INDATA-SW                          
527099                                       ALLT-SW                            
527199       ELSE                                                               
527399         MOVE MID-IDDISTR-UPD       TO MOD-IDDISTR-UPD                    
527499         MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDDISTR-UPD-ATTR               
528499       END-IF                                                             
528599                                                                          
528699*   KONTROLLERA KONTO                                                     
528799       MOVE MID-KDEXCHA-UPD         TO WS-KDEXCHA-ALFA                    
528899       INSPECT WS-KDEXCHA-ALFA REPLACING ALL ',' BY '0'                   
528999       INSPECT WS-KDEXCHA-ALFA REPLACING ALL '.' BY '0'                   
529099       INSPECT WS-KDEXCHA-ALFA REPLACING ALL '-' BY '0'                   
529199       INSPECT WS-KDEXCHA-ALFA REPLACING ALL '+' BY '0'                   
529299       INSPECT WS-KDEXCHA-ALFA REPLACING ALL '!' BY '0'                   
529399       INSPECT WS-KDEXCHA-ALFA REPLACING ALL '"' BY '0'                   
529499       INSPECT WS-KDEXCHA-ALFA REPLACING ALL ' ' BY '0'                   
529599       INSPECT WS-KDEXCHA-ALFA REPLACING ALL '%' BY '0'                   
529699       INSPECT WS-KDEXCHA-ALFA REPLACING ALL '&' BY '0'                   
529799       INSPECT WS-KDEXCHA-ALFA REPLACING ALL '/' BY '0'                   
529899       INSPECT WS-KDEXCHA-ALFA REPLACING ALL '(' BY '0'                   
529999       INSPECT WS-KDEXCHA-ALFA REPLACING ALL ')' BY '0'                   
530099       INSPECT WS-KDEXCHA-ALFA REPLACING ALL '?' BY '0'                   
530199       INSPECT WS-KDEXCHA-ALFA REPLACING ALL ':' BY '0'                   
530299       INSPECT WS-KDEXCHA-ALFA REPLACING ALL '_' BY '0'                   
530399       INSPECT WS-KDEXCHA-ALFA REPLACING ALL '*' BY '0'                   
530499       IF MID-KDEXCHA-UPD = ALL '+' OR                                    
530599          WS-KDEXCHA-ALFA NOT NUMERIC                                     
530799         MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                       
530899         MOVE MFS-NUM-FAELT-FEL     TO MOD-KDEXCHA-UPD-ATTR               
530999         MOVE NEJ                   TO INDATA-SW                          
531099                                       ALLT-SW                            
531199       ELSE                                                               
531299         MOVE WS-KDEXCHA-ALFA       TO MOD-KDEXCHA-UPD                    
531399         MOVE MFS-NUM-FAELT-RAETT   TO MOD-KDEXCHA-UPD-ATTR               
531499       END-IF                                                             
531599                                                                          
531699*   KONTROLLERA RADNR                                                     
531799       MOVE MID-IDBYTRAD-UPD      TO WS-RAD-ALFA                          
531899       INSPECT WS-RAD-ALFA REPLACING ALL ',' BY '0'                       
531999       INSPECT WS-RAD-ALFA REPLACING ALL '.' BY '0'                       
532099       INSPECT WS-RAD-ALFA REPLACING ALL '-' BY '0'                       
532199       INSPECT WS-RAD-ALFA REPLACING ALL '+' BY '0'                       
532299       INSPECT WS-RAD-ALFA REPLACING ALL '!' BY '0'                       
532399       INSPECT WS-RAD-ALFA REPLACING ALL '"' BY '0'                       
532499       INSPECT WS-RAD-ALFA REPLACING ALL ' ' BY '0'                       
532599       INSPECT WS-RAD-ALFA REPLACING ALL '%' BY '0'                       
532699       INSPECT WS-RAD-ALFA REPLACING ALL '&' BY '0'                       
532799       INSPECT WS-RAD-ALFA REPLACING ALL '/' BY '0'                       
532899       INSPECT WS-RAD-ALFA REPLACING ALL '(' BY '0'                       
532999       INSPECT WS-RAD-ALFA REPLACING ALL ')' BY '0'                       
533099       INSPECT WS-RAD-ALFA REPLACING ALL '?' BY '0'                       
533199       INSPECT WS-RAD-ALFA REPLACING ALL ':' BY '0'                       
533299       INSPECT WS-RAD-ALFA REPLACING ALL '_' BY '0'                       
533399       INSPECT WS-RAD-ALFA REPLACING ALL '*' BY '0'                       
533499       IF MID-CMD = 'C' OR 'D'                                            
533599         IF MID-IDBYTRAD-UPD = ALL '+' OR                                 
533699            WS-RAD-ALFA NOT NUMERIC                                       
533799           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
533899           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDBYTRAD-UPD-ATTR            
533999           MOVE NEJ                   TO INDATA-SW                        
534099                                         ALLT-SW                          
534199         ELSE                                                             
534299           MOVE WS-RAD-ALFA           TO MOD-IDBYTRAD-UPD                 
534399           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDBYTRAD-UPD-ATTR            
534499         END-IF                                                           
534599       END-IF                                                             
534699       IF MID-CMD = 'N'                                                   
534799         IF MID-IDBYTRAD-UPD NOT = ALL '+'                                
534899           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
534999           MOVE MFS-NUM-FAELT-FEL     TO MOD-IDBYTRAD-UPD-ATTR            
535099           MOVE NEJ                   TO INDATA-SW                        
535199                                         ALLT-SW                          
535299         ELSE                                                             
535399           MOVE ZERO                  TO MOD-IDBYTRAD-UPD                 
535499           MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDBYTRAD-UPD-ATTR            
535599         END-IF                                                           
535699       END-IF                                                             
535799                                                                          
535899*   KONTROLLERA BESKRIVNING                                               
535999       IF MID-CMD = 'C' OR 'N'                                            
536099         IF MID-TENOTE-UPD = ALL '+'                                      
536199           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
536299           MOVE MFS-ALFA-FAELT-FEL    TO MOD-TENOTE-UPD-ATTR              
536399           MOVE NEJ                   TO INDATA-SW                        
536499                                         ALLT-SW                          
536599         ELSE                                                             
536699           MOVE MID-TENOTE-UPD        TO MOD-TENOTE-UPD                   
536799           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TENOTE-UPD-ATTR              
536899         END-IF                                                           
536999       END-IF                                                             
537099                                                                          
537199*   KONTROLLERA POÄNG                                                     
537299       IF MID-CMD = 'C' OR 'N'                                            
537399         MOVE MID-KVPOINT-UPD         TO WS-KVPOINT-ALFA                  
537499         INSPECT WS-KVPOINT-ALFA REPLACING ALL ',' BY '0'                 
537599         INSPECT WS-KVPOINT-ALFA REPLACING ALL '.' BY '0'                 
537699         INSPECT WS-KVPOINT-ALFA REPLACING ALL '-' BY '0'                 
537799         INSPECT WS-KVPOINT-ALFA REPLACING ALL '+' BY '0'                 
537899         INSPECT WS-KVPOINT-ALFA REPLACING ALL '!' BY '0'                 
537999         INSPECT WS-KVPOINT-ALFA REPLACING ALL '"' BY '0'                 
538099         INSPECT WS-KVPOINT-ALFA REPLACING ALL ' ' BY '0'                 
538199         INSPECT WS-KVPOINT-ALFA REPLACING ALL '%' BY '0'                 
538299         INSPECT WS-KVPOINT-ALFA REPLACING ALL '&' BY '0'                 
538399         INSPECT WS-KVPOINT-ALFA REPLACING ALL '/' BY '0'                 
538499         INSPECT WS-KVPOINT-ALFA REPLACING ALL '(' BY '0'                 
538599         INSPECT WS-KVPOINT-ALFA REPLACING ALL ')' BY '0'                 
538699         INSPECT WS-KVPOINT-ALFA REPLACING ALL '?' BY '0'                 
538799         INSPECT WS-KVPOINT-ALFA REPLACING ALL ':' BY '0'                 
538899         INSPECT WS-KVPOINT-ALFA REPLACING ALL '_' BY '0'                 
538999         INSPECT WS-KVPOINT-ALFA REPLACING ALL '*' BY '0'                 
539099         IF MID-KVPOINT-UPD = ALL '+' OR                                  
539199            WS-KVPOINT-ALFA NOT NUMERIC                                   
539299           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
539399           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVPOINT-UPD-ATTR             
539499           MOVE NEJ                   TO INDATA-SW                        
539599                                         ALLT-SW                          
539699         ELSE                                                             
539799           MOVE WS-KVPOINT-ALFA       TO MOD-KVPOINT-UPD                  
539899           MOVE MFS-NUM-FAELT-RAETT   TO MOD-KVPOINT-UPD-ATTR             
539999         END-IF                                                           
540099       END-IF                                                             
540499                                                                          
540599*   KONTROLLERA DEBET/KREDIT                                              
540699       IF MID-CMD = 'C' OR 'N'                                            
540799         IF MID-FLDEBCRE-UPD = 'D' OR 'C'                                 
540899           MOVE MID-FLDEBCRE-UPD      TO MOD-FLDEBCRE-UPD                 
540999           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-FLDEBCRE-UPD-ATTR            
541499         ELSE                                                             
541599           MOVE ERR-CORR-HILITE-FLDS  TO MED-IDMFSFEL                     
541699           MOVE MFS-ALFA-FAELT-FEL    TO MOD-FLDEBCRE-UPD-ATTR            
541799           MOVE NEJ                   TO INDATA-SW                        
541899                                         ALLT-SW                          
541999         END-IF                                                           
542099       END-IF                                                             
542199                                                                          
542299       IF ALLT-OK                                                         
542399*---KONTROLLERA SÅ ATT DISTRIKT FINNS I WDB2                              
542499         MOVE MID-IDDISTR-UPD       TO W-IDDISTR-WDB-MIN                  
542599                                       W-IDDISTR-WDB-MAX                  
542699         PERFORM IMS-GU-WDB201                                            
542799         IF SEGMENT-SAKNAS                                                
542899           MOVE ERR-DIST-MISSING    TO MED-IDMFSFEL                       
542999           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-UPD-ATTR               
543099           MOVE NEJ                 TO INDATA-SW                          
543199                                       ALLT-SW                            
543299         ELSE                                                             
545499                                                                          
545599           IF MID-CMD = 'C'                                               
545699*---KONTROLLERA SÅ SEGMENT FINNS PÅ BAS                                   
545799             MOVE MID-IDDISTR-UPD        TO W-IDDISTR-A                   
545999             MOVE WS-KDEXCHA-ALFA        TO W-KDEXCHA-A                   
546099             MOVE WS-RAD-ALFA            TO W-IDBYTRAD-A                  
546199             PERFORM IMS-GU-WDA701-ASEQ                                   
546299             IF SEGMENT-SAKNAS                                            
546399               MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-UPD-ATTR          
546499                                            MOD-KDEXCHA-UPD-ATTR          
546599                                            MOD-IDBYTRAD-UPD-ATTR         
546699               MOVE NEJ                  TO INDATA-SW                     
546799               MOVE JA                   TO TEXT-SW                       
546899               MOVE '999 ITEM MISSING'   TO MOD-TEMFSFEL                  
546999             ELSE                                                         
547099               MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-UPD-ATTR          
547199                                            MOD-KDEXCHA-UPD-ATTR          
547299                                            MOD-IDBYTRAD-UPD-ATTR         
547399             END-IF                                                       
547499           END-IF                                                         
547599                                                                          
547699           IF MID-CMD = 'D'                                               
547799*---KONTROLLERA SÅ SEGMENT FINNS PÅ BAS                                   
547899             MOVE MID-IDDISTR-UPD        TO W-IDDISTR-A                   
547999             MOVE WS-KDEXCHA-ALFA        TO W-KDEXCHA-A                   
548099             MOVE WS-RAD-ALFA            TO W-IDBYTRAD-A                  
548199             PERFORM IMS-GU-WDA701-ASEQ                                   
548299             IF SEGMENT-SAKNAS                                            
548399               MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-UPD-ATTR          
548499                                            MOD-KDEXCHA-UPD-ATTR          
548599                                            MOD-IDBYTRAD-UPD-ATTR         
548699               MOVE NEJ                  TO INDATA-SW                     
548799               MOVE JA                   TO TEXT-SW                       
548899               MOVE '999 ITEM MISSING'   TO MOD-TEMFSFEL                  
548999           MOVE WS-KDEXCHA             TO MOD-TEMFSFEL                    
549099             ELSE                                                         
549199               MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-UPD-ATTR          
549299                                            MOD-KDEXCHA-UPD-ATTR          
549399                                            MOD-IDBYTRAD-UPD-ATTR         
549499             END-IF                                                       
549599           END-IF                                                         
549699         END-IF                                                           
549799       END-IF                                                             
549899                                                                          
549999       IF INDATA-FEL                                                      
550099         IF NOT TEXT-KLAR                                                 
550199           CALL WMEDKONV USING MED-WMEDAREA                               
550299           MOVE MED-MFSFEL             TO MOD-TEMFSFEL                    
550499         END-IF                                                           
550599         PERFORM MFS-ROER-EJ-FAELT-UT                                     
550699         PERFORM MFS-ROER-EJ-FAELT-IN                                     
550799         MOVE NEJ                      TO ALLT-SW                         
550899       END-IF                                                             
550999     END-IF                                                               
551099     .                                                                    
551199     EJECT                                                                
551299                                                                          
551399 H-UPPDATERA SECTION.                                                     
551499     MOVE MID-IDDISTR-UPD         TO W-IDDISTR                            
551599     MOVE WS-RAD-ALFA             TO W-IDBYTRAD                           
551699     PERFORM IMS-GHU-WDA701                                               
551799                                                                          
551899     IF SEGMENT-FINNS                                                     
551999       IF MID-CMD = 'D'                                                   
552099         PERFORM IMS-DLET-WDA701                                          
552199                                                                          
552299       ELSE                                                               
552399         IF MID-CMD = 'C'                                                 
552499           MOVE MID-TENOTE-UPD    TO ROT-TENOTE                           
552599           MOVE WS-KVPOINT-ALFA   TO WS-KVPOINT                           
552699           IF MID-FLDEBCRE-UPD = 'C'                                      
552799             COMPUTE ROT-KVPOINT = WS-KVPOINT * -1                        
552899             END-COMPUTE                                                  
552999           ELSE                                                           
553099             MOVE WS-KVPOINT      TO ROT-KVPOINT                          
553199           END-IF                                                         
553299           MOVE MSGI-IDUSER       TO ROT-IDUSER                           
553399           MOVE DAGENS-DATUM      TO ROT-DAREGDAT                         
553499                                     ROT-DAFAKT                           
553599           MOVE DAGENS-TID        TO ROT-TIKLOCK                          
553699           PERFORM IMS-REPL-WDA701                                        
553799         END-IF                                                           
553899       END-IF                                                             
553999                                                                          
554099     ELSE                                                                 
554199       IF MID-CMD = 'N'                                                   
554299         MOVE 'ADJ'               TO ROT-IDPTYP                           
554399         MOVE MID-IDDISTR-UPD     TO ROT-IDDISTR                          
554499         MOVE WS-KDEXCHA-ALFA     TO ROT-KDEXCHA                          
554599         MOVE +1                  TO ROT-IDBYTRAD                         
554699         MOVE SPACE               TO ROT-IDDC                             
554799         MOVE ZERO                TO ROT-DAFAKT                           
554899                                     ROT-IDARTNR-BYT                      
554999                                     ROT-IDKUNDNR                         
555099                                     ROT-IDORDER                          
555199                                     ROT-KVANTAL                          
555299         MOVE MID-TENOTE-UPD      TO ROT-TENOTE                           
555399         MOVE WS-KVPOINT-ALFA     TO WS-KVPOINT                           
555499         IF MID-FLDEBCRE-UPD = 'C'                                        
555599           COMPUTE ROT-KVPOINT = WS-KVPOINT * -1                          
555699           END-COMPUTE                                                    
555799         ELSE                                                             
555899           MOVE WS-KVPOINT        TO ROT-KVPOINT                          
555999         END-IF                                                           
556099         MOVE MSGI-IDUSER         TO ROT-IDUSER                           
556199         MOVE DAGENS-DATUM        TO ROT-DAREGDAT                         
556299                                     ROT-DAFAKT                           
556399         MOVE DAGENS-TID          TO ROT-TIKLOCK                          
556499         PERFORM IMS-ISRT-WDA701                                          
556599         IF SEGMENT-FINNS-REDAN OR                                        
556699            INDEX-FINNS-REDAN                                             
556799           PERFORM UNTIL NOT INDEX-FINNS-REDAN AND                        
556899                         NOT SEGMENT-FINNS-REDAN                          
556999             ADD +1               TO ROT-IDBYTRAD                         
557099             PERFORM IMS-ISRT-WDA701                                      
557199           END-PERFORM                                                    
557299         END-IF                                                           
557399       END-IF                                                             
557499     END-IF                                                               
557599                                                                          
557699     MOVE INF-UPDATE-DONE         TO MED-IDMFSINF                         
557799     CALL WMEDKONV USING MED-WMEDAREA                                     
557899     MOVE MED-MFSINF              TO MOD-TEMFSINF                         
557999     PERFORM MFS-FORM-ATTR                                                
558099     PERFORM MFS-RENSA-FAELT-IN                                           
558199     .                                                                    
558299     EJECT                                                                
558399                                                                          
559099 MFS-RENSA-FAELT-UT SECTION.                                              
560000*    --- ALLA UTDATA-FÄLT                                                 
571099*    --- INKL. BLÄDDRINGSRADER                                            
580002     MOVE MFS-RENSA-FAELT TO MOD-CMD                                      
590099                             MOD-IDDISTR-UPD                              
591099                             MOD-KDEXCHA-UPD                              
591199                             MOD-IDBYTRAD-UPD                             
591299                             MOD-TENOTE-UPD                               
591399                             MOD-KVPOINT-UPD                              
591499                             MOD-FLDEBCRE-UPD                             
592099                                                                          
592100     MOVE +1 TO INDX                                                      
593199     PERFORM UNTIL INDX > MAX-INDX                                        
594002       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
595002       ADD +1 TO INDX                                                     
596002     END-PERFORM                                                          
600000     .                                                                    
610199                                                                          
610200 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
610400*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
610599     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR (INDX)                           
610699                             MOD-KDEXCHA (INDX)                           
610799                             MOD-IDBYTRAD (INDX)                          
610899                             MOD-TENOTE (INDX)                            
610999                             MOD-KVPOINT (INDX)                           
611099                             MOD-IDUSER (INDX)                            
611199                             MOD-DAREGDAT (INDX)                          
612099     .                                                                    
620099                                                                          
630000 MFS-RENSA-FAELT-IN SECTION.                                              
650000*    --- ALLA INDATA-FÄLT                                                 
660099     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
661099                             MOD-KDEXCHA-IN                               
670002                             MOD-CMD                                      
671099                             MOD-IDDISTR-UPD                              
672099                             MOD-KDEXCHA-UPD                              
673099                             MOD-IDBYTRAD-UPD                             
673199                             MOD-TENOTE-UPD                               
674099                             MOD-KVPOINT-UPD                              
675099                             MOD-FLDEBCRE-UPD                             
680000     .                                                                    
690099                                                                          
700000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
720000*    --- ALLA UTDATA-FÄLT                                                 
731000*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
740002     MOVE MFS-ROER-EJ-FAELT TO MOD-CMD                                    
750099                               MOD-IDDISTR-UPD                            
751099                               MOD-KDEXCHA-UPD                            
760099                               MOD-IDBYTRAD-UPD                           
760199                               MOD-TENOTE-UPD                             
760299                               MOD-KVPOINT-UPD                            
760399                               MOD-FLDEBCRE-UPD                           
760499                                                                          
760599     MOVE +1 TO INDX                                                      
760699     PERFORM UNTIL INDX > MAX-INDX                                        
760799       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
760899       ADD +1 TO INDX                                                     
760999     END-PERFORM                                                          
761099     .                                                                    
761199                                                                          
761299 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
761399*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
761499     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR (INDX)                         
761599                               MOD-KDEXCHA (INDX)                         
761699                               MOD-IDBYTRAD (INDX)                        
761799                               MOD-TENOTE (INDX)                          
761899                               MOD-KVPOINT (INDX)                         
761999                               MOD-IDUSER (INDX)                          
762099                               MOD-DAREGDAT (INDX)                        
763099     .                                                                    
780099                                                                          
790000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
810000*    --- ALLA INDATA-FÄLT                                                 
820099     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-IN                             
821099                               MOD-KDEXCHA-IN                             
830002                               MOD-CMD                                    
831099                               MOD-IDDISTR-UPD                            
832099                               MOD-KDEXCHA-UPD                            
833099                               MOD-IDBYTRAD-UPD                           
834099                               MOD-TENOTE-UPD                             
835099                               MOD-KVPOINT-UPD                            
836099                               MOD-FLDEBCRE-UPD                           
840000     .                                                                    
850099                                                                          
860000 MFS-FORM-ATTR SECTION.                                                   
880000*    --- ALLA INDATA-FÄLT                                                 
890002     MOVE MFS-FORMATETS-ATTR TO MOD-CMD-ATTR                              
900099                                MOD-IDDISTR-UPD-ATTR                      
901099                                MOD-KDEXCHA-UPD-ATTR                      
902099                                MOD-IDBYTRAD-UPD-ATTR                     
903099                                MOD-TENOTE-UPD-ATTR                       
904099                                MOD-KVPOINT-UPD-ATTR                      
905099                                MOD-FLDEBCRE-UPD-ATTR                     
910000     .                                                                    
920099                                                                          
930066 MFS-LAES-IN-IGEN SECTION.                                                
950000*    --- ALLA INDATA-FÄLT                                                 
960066     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-CMD-ATTR                           
970099                                   MOD-IDDISTR-UPD-ATTR                   
971099                                   MOD-KDEXCHA-UPD-ATTR                   
972099                                   MOD-IDBYTRAD-UPD-ATTR                  
973099                                   MOD-TENOTE-UPD-ATTR                    
974099                                   MOD-KVPOINT-UPD-ATTR                   
975099                                   MOD-FLDEBCRE-UPD-ATTR                  
980066     .                                                                    
990066     EJECT                                                                
991099                                                                          
000000* --- IMS SEKTIONER ---                                                   
010099                                                                          
020000 IMS-GET-MSG SECTION.                                                     
040099     MOVE '  QC'          TO GODK-STATUSKODER                             
050000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
060000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070000     PERFORM IMS-STATUSKONTROLL                                           
080000     .                                                                    
090099                                                                          
100000 IMS-INSERT-MSG SECTION.                                                  
110099     IF MSGI-IDLAND-SPR = 'GB'                                            
120099       MOVE 'N'           TO MFS-KDHUVOMR                                 
130099     END-IF                                                               
150000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
160099     MOVE SPACE           TO GODK-STATUSKODER                             
170000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
180000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
190000     PERFORM IMS-STATUSKONTROLL                                           
200000     .                                                                    
210100     EJECT                                                                
210299                                                                          
217499 IMS-GU-WDA701-ASEQ SECTION.                                              
217599     STRING 'WDA701  (WDA7ASEQ =' W-WDA7ASEQ-X ')'                        
217699     DELIMITED BY SIZE INTO SSA1                                          
218299     MOVE '  GE'            TO GODK-STATUSKODER                           
218399     CALL CBLTDLI USING GU WDA7A-PCB DLI-IO-WDA701 SSA1                   
218499     MOVE WDA7A-STATUS-CODE TO STATUS-WS                                  
218599     PERFORM IMS-STATUSKONTROLL                                           
219099     .                                                                    
219199                                                                          
219299 IMS-GN-WDA701-ASEQ SECTION.                                              
219399     STRING 'WDA701  (WDA7ASEQ>=' W-WDA7ASEQ-MIN-X                        
219499                    '&WDA7ASEQ<=' W-WDA7ASEQ-MAX-X ')'                    
219599     DELIMITED BY SIZE INTO SSA1                                          
219899     MOVE '  GEGB'          TO GODK-STATUSKODER                           
219999     CALL CBLTDLI USING GN WDA7A-PCB DLI-IO-WDA701 SSA1                   
220099     MOVE WDA7A-STATUS-CODE TO STATUS-WS                                  
220199     PERFORM IMS-STATUSKONTROLL                                           
220299     .                                                                    
220399     EJECT                                                                
220499                                                                          
222499 IMS-GHU-WDA701 SECTION.                                                  
222599     STRING 'WDA701  (WDA701KY =' W-WDA701KY-X ')'                        
222699     DELIMITED BY SIZE INTO SSA1                                          
222799     MOVE '  GE'           TO GODK-STATUSKODER                            
222899     CALL CBLTDLI USING GHU WDA7-PCB DLI-IO-WDA701 SSA1                   
222999     MOVE WDA7-STATUS-CODE TO STATUS-WS                                   
223099     PERFORM IMS-STATUSKONTROLL                                           
223199     .                                                                    
224099                                                                          
227199 IMS-ISRT-WDA701 SECTION.                                                 
227299     MOVE 'WDA701   '      TO SSA1                                        
227399     MOVE '  IINI'         TO GODK-STATUSKODER                            
227499     CALL CBLTDLI USING ISRT WDA7-PCB DLI-IO-WDA701 SSA1                  
227599     MOVE WDA7-STATUS-CODE TO STATUS-WS                                   
227699     PERFORM IMS-STATUSKONTROLL                                           
227799     .                                                                    
227899                                                                          
227999 IMS-REPL-WDA701 SECTION.                                                 
228099     MOVE '  '             TO GODK-STATUSKODER                            
228199     CALL CBLTDLI USING REPL WDA7-PCB DLI-IO-WDA701                       
228299     MOVE WDA7-STATUS-CODE TO STATUS-WS                                   
228399     PERFORM IMS-STATUSKONTROLL                                           
228499     .                                                                    
228599                                                                          
228699 IMS-DLET-WDA701 SECTION.                                                 
228799     MOVE '  '             TO GODK-STATUSKODER                            
228899     CALL CBLTDLI USING DLET WDA7-PCB DLI-IO-WDA701                       
228999     MOVE WDA7-STATUS-CODE TO STATUS-WS                                   
229099     PERFORM IMS-STATUSKONTROLL                                           
229199     .                                                                    
229299     EJECT                                                                
229399                                                                          
229499 IMS-GU-WDB201 SECTION.                                                   
230399     STRING 'WDB201  (IDGMT   >=' W-WDB201KEY-MIN-X                       
230499                    '&IDGMT   <=' W-WDB201KEY-MAX-X ')'                   
230599            DELIMITED BY SIZE INTO SSA1                                   
230699     MOVE '  GE' TO GODK-STATUSKODER                                      
230799     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
230899     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
230999     PERFORM IMS-STATUSKONTROLL                                           
231199     .                                                                    
231299     EJECT                                                                
231399                                                                          
232000 IMS-STATUSKONTROLL SECTION.                                              
250000     SET STATUS-IX TO 1                                                   
260000     SEARCH GODK-STATUS                                                   
270000       AT END                                                             
280000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
290000         DELIMITED BY SIZE INTO FELTEXT                                   
300000         CALL FELLOG                                                      
310000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
320000         CONTINUE                                                         
330000     END-SEARCH                                                           
340000     .                                                                    
