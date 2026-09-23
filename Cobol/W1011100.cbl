001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W1011100.                                                
001300*AUTHOR.         BODIL LINDAHL.                                           
001400*DATE-WRITTEN.   93/12/16.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        UPPDATERING AV STATNUMMER WDK611 WLARTC11                        
002000*                                                                         
002102*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002103*        PROGRAMMET LÄSER      WLARTG (WDD2)                              
002110*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W1T111                                              
002410*                     W1T111U                                             
002420*                     W1T111X                                             
002500*        MID:         W1I11101                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W1O11101                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400 WORKING-STORAGE SECTION.                                                 
003401                                                                          
003410*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'W1011100'.            
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004501 77  STAT-IX                     PIC S9(3)  VALUE +0    COMP-3.           
004510 77  STAT-IX-MAX                 PIC S9(3)  VALUE +5    COMP-3.           
004600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004700 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +229  COMP SYNC.        
005010 77  WS-IDARTNR                  PIC X(9)   VALUE SPACE.                  
005011                                                                          
005020 01  WS-IDSTATNR                 PIC 9(9).                                
005030 01  FILLER REDEFINES WS-IDSTATNR.                                        
005040     03  WS-IDSTATNR-POS1        PIC 9.                                   
005050     03  WS-IDSTATNR-POS2        PIC 9.                                   
005060     03  WS-IDSTATNR-POS3-9.                                              
005070         05  WS-IDSTATNR-POS3-8  PIC 9(6).                                
005080         05  WS-IDSTATNR-POS9    PIC 9.                                   
005300                                                                          
005310 01  WS-IDSTATNR-GB              PIC 9(9)   VALUE ZERO.                   
005320                                                                          
005330 01  WS-IDSTATNR-BELGIEN         PIC 9(9).                                
005331 01  FILLER REDEFINES WS-IDSTATNR-BELGIEN.                                
005332     03 WS-BELGIEN-POS1          PIC 9.                                   
005333     03 WS-BELGIEN-POS2-8        PIC 9(7).                                
005334     03 WS-BELGIEN-POS9          PIC 9.                                   
005340                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005700                                                                          
005710 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005720     88  INDATA-OK                           VALUE 'J'.                   
005730     88  INDATA-FEL                          VALUE 'N'.                   
005740                                                                          
005800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005900     88  EGEN-MID                            VALUE '1111'.                
006500     88  HELP-MID                            VALUE '0551'.                
006600     EJECT                                                                
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
006900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007300     EJECT                                                                
007310*                   ****    PARAMETRAR TILL W005INIT                      
007320*01  -COPY WMSGINIT                                                       
007330     EJECT                                                                
007400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007500*01 -COPY WMEDAREA                                                        
007600     SKIP3                                                                
007700 01  MESSAGE-CODES.                                                       
008000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008100     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
008101     03  ARTIKEL-UTGANGEN        PIC X(3)    VALUE '018'.                 
008110     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008120     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008130     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008140     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008200     EJECT                                                                
008300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008600     SKIP3                                                                
008700*01  MID -COPY W1I11101                                                   
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)  VALUE 'MSG-KOM-AREA'.         
009000     SKIP3                                                                
009100*01  -COPY WMSGKOM                                                        
009110 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009120     EJECT                                                                
009130*01  -COPY WMSGAREA                                                       
009200     EJECT                                                                
009300     03  MOD REDEFINES MSG-AREA.                                          
009400*      05  -COPY W1O11101                                                 
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009700     SKIP3                                                                
009800*01  -COPY WMFSAREA                                                       
009900     EJECT                                                                
010000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010100*                                                                         
010200     SKIP3                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010400     SKIP3                                                                
010500 01  NYCKLAR-TILL-DLI.                                                    
010601     03  W-IDARTNR-X.                                                     
010602         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010607     03  W-IDSKYLT-X.                                                     
010610         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
010700     SKIP2                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011300     SKIP2                                                                
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600     SKIP3                                                                
011700 01  SSA1                        PIC X(64).                               
011800 01  SSA2                        PIC X(64).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012300     EJECT                                                                
012400*    ---  DLI INPUT-OUTPUT AREA                                           
012500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012600     SKIP3                                                                
012700 01  DLI-IO-AREA.                                                         
012800     03  IO-AREA                 PIC X(600)  VALUE SPACE.                 
012901     SKIP3                                                                
012908     03  WLARTG01 REDEFINES IO-AREA.                                      
012909*        05  -COPY WDD201  -PRE ARTG01-                                   
012910     EJECT                                                                
012911     03  WLBENA11 REDEFINES IO-AREA.                                      
012920*        05  -COPY WDD311  -PRE BENA11-                                   
013200     EJECT                                                                
013210 01  DLI-IO-AREA-2.                                                       
013220     03  IO-AREA-2               PIC X(928)  VALUE SPACE.                 
013230     SKIP3                                                                
013270     03  WLARTC01 REDEFINES IO-AREA-2.                                    
013280*        05  -COPY WDK601                                                 
013290     EJECT                                                                
013291     03  WLARTC11 REDEFINES IO-AREA-2.                                    
013292*        05  -COPY WDK611                                                 
013293     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400                                                                          
013500*01  -COPY W0009   -PRE MSG-                                              
013603     EJECT                                                                
013604*01  -COPY W0009   -PRE MSGKOM-                                           
013605     EJECT                                                                
013606*01  -COPY W0008     -PRE USEA-                                           
013607         05  FILLER           PIC X.                                      
013608     EJECT                                                                
013609*01  -COPY W0008  -PRE ARTG-                                              
013610     05  FILLER                  PIC X.                                   
013611     EJECT                                                                
013612*01  -COPY W0008  -PRE BENA-                                              
013620     05  FILLER                  PIC X.                                   
013700     EJECT                                                                
013800*01  -COPY W0008  -PRE ARTC-                                              
013801     05  FILLER                  PIC X.                                   
013802     EJECT                                                                
013803 PROCEDURE DIVISION  USING MSG-PCB MSGKOM-PCB USEA-PCB                    
013804      ARTG-PCB BENA-PCB ARTC-PCB.                                         
013805     ENTRY 'DLITCBL' USING MSG-PCB MSGKOM-PCB USEA-PCB                    
013810      ARTG-PCB BENA-PCB ARTC-PCB.                                         
013900                                                                          
014100     PERFORM IMS-GET-MSG                                                  
014200     IF SEGMENT-FINNS                                                     
014210       PERFORM IMS-GET-WMSGKOM                                            
014300       PERFORM A-INIT                                                     
014400       PERFORM B-KOLLA-NYCKLAR                                            
014500       IF NYCKLAR-OK                                                      
014701          IF MFS-UPDATE OR MFS-UPD-X                                      
014702             PERFORM G-KOLLA-INPUT                                        
014703             IF INDATA-OK                                                 
014704                PERFORM H-UPPDATERA                                       
014705             END-IF                                                       
014706          ELSE                                                            
014707            IF MFS-FIRST                                                  
014708              PERFORM C-FOERSTA-SIDA                                      
014709            ELSE                                                          
014710              PERFORM E-SAMMA-SIDA                                        
014720            END-IF                                                        
014730          END-IF                                                          
015000          PERFORM F-LAES-VISA-INFO                                        
015100       END-IF                                                             
015110                                                                          
015120       IF MFS-UPD-X                                                       
015130          PERFORM IMS-INSERT-WMSGKOM                                      
015140       ELSE                                                               
015200          MOVE MAX-MOD-LAENGD TO MSG-KVLL                                 
015300          PERFORM IMS-INSERT-MSG                                          
015310       END-IF                                                             
015400     END-IF                                                               
015600                                                                          
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100 A-INIT SECTION.                                                          
016200                                                                          
016300     IF MSG-DUBBLA-TRANSKODER                                             
016400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I11101                 
016500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
016600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
016700     ELSE                                                                 
016800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I11101                  
016900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017100     END-IF                                                               
017200                                                                          
017300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
017600                                                                          
017700     MOVE LOW-VALUE TO MSG-AREA                                           
017800     MOVE 'W1O111N1' TO MFS-IDMOD                                         
017900     MOVE '1111' TO MOD-IDTRANS                                           
018000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018100                                                                          
018200     IF EGEN-MID OR HELP-MID                                              
018300       CONTINUE                                                           
018400     ELSE                                                                 
018500       MOVE SPACE TO MFS-KDTRTYP                                          
018600       MOVE '7' TO MFS-IDPFK                                              
018700     END-IF                                                               
019800     .                                                                    
019900     EJECT                                                                
020000 B-KOLLA-NYCKLAR SECTION.                                                 
020100                                                                          
020200     MOVE JA TO NYCKLAR-SW                                                
020303     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
020304                                                                          
020305     IF MFS-UPD-X                                                         
020306****************  DISPATCHANROP                                           
020307                                                                          
020308       IF MID-IDARTNR-IN = ALL '+'                                        
020309         MOVE MID-IDARTNR-UT TO WS-IDARTNR                                
020310         INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO               
020311       ELSE                                                               
020312         MOVE MID-IDARTNR-IN TO WS-IDARTNR                                
020313         MOVE '7'         TO MFS-IDPFK                                    
020314         MOVE SPACE       TO MFS-KDTRTYP                                  
020315       END-IF                                                             
020316                                                                          
020317     ELSE                                                                 
020318       IF MID-IDARTNR-IN = ALL '+'                                        
020319         CONTINUE                                                         
020321       ELSE                                                               
020323         MOVE '7'         TO MFS-IDPFK                                    
020324         MOVE SPACE       TO MFS-KDTRTYP                                  
020325       END-IF                                                             
020326       MOVE ALL '+' TO MSGI-WMSGINIT                                      
020327       MOVE '001'             TO MSGI-KDCALL                              
020328       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
020329       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
020330       MOVE '1111'            TO MSGI-IDTRANS                             
020331       IF MFS-IDTRANS = '1111'                                            
020332       OR (MID-IDARTNR-IN NUMERIC                                         
020333       AND MID-IDARTNR-IN > ZERO)                                         
020334           MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                            
020335       END-IF                                                             
020336       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
020337       MOVE MSGI-IDARTNR TO WS-IDARTNR                                    
020338       INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                     
020339     END-IF                                                               
020340                                                                          
020341     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
020342       MOVE WS-IDARTNR TO W-IDARTNR                                       
020343     ELSE                                                                 
020344       MOVE NEJ TO NYCKLAR-SW                                             
020350     END-IF                                                               
020351                                                                          
020360     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
020370       MOVE +1 TO SPRAK-IX                                                
020380       MOVE 'S  ' TO MED-IDSKYLT                                          
020381                     W-IDSKYLT                                            
020390     ELSE                                                                 
020400       MOVE +2 TO SPRAK-IX                                                
020401       MOVE 'GB ' TO MED-IDSKYLT                                          
020402                     W-IDSKYLT                                            
020403     END-IF                                                               
020404                                                                          
020405     IF NYCKLAR-OK                                                        
020406       MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
020407       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
020408     ELSE                                                                 
020409       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
020410     END-IF                                                               
020500                                                                          
020600     IF NYCKLAR-FEL                                                       
020700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
020800       CALL WMEDKONV USING MED-WMEDAREA                                   
020900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
020910                          MSG-KOM-IDMFSMED                                
021000       PERFORM MFS-RENSA-FAELT-IN                                         
021100       PERFORM MFS-RENSA-FAELT-UT                                         
021110       PERFORM MFS-RENSA-IDSTATNR-UT                                      
021200     END-IF                                                               
021300     .                                                                    
021401     EJECT                                                                
021402 C-FOERSTA-SIDA SECTION.                                                  
021403                                                                          
021410     PERFORM MFS-RENSA-FAELT-IN                                           
021411     .                                                                    
021412     EJECT                                                                
021419 E-SAMMA-SIDA SECTION.                                                    
021420                                                                          
021421     IF EGEN-MID OR HELP-MID                                              
021423       IF MID-INPUT = ALL '+'                                             
021424         PERFORM MFS-RENSA-FAELT-IN                                       
021425       ELSE                                                               
021426         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
021427         CALL WMEDKONV USING MED-WMEDAREA                                 
021428         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
021429         PERFORM EA-MID-INDATA-TILL-MOD                                   
021430       END-IF                                                             
021431     ELSE                                                                 
021432       PERFORM MFS-RENSA-FAELT-IN                                         
021433     END-IF                                                               
021434     .                                                                    
021435     EJECT                                                                
021436 EA-MID-INDATA-TILL-MOD SECTION.                                          
021437                                                                          
021438     MOVE +1 TO STAT-IX                                                   
021439     PERFORM UNTIL STAT-IX > STAT-IX-MAX                                  
021440        IF MID-IDSTATNR(STAT-IX) = ALL '+'                                
021441           MOVE MFS-RENSA-FAELT TO MOD-IDSTATNR-IN(STAT-IX)               
021442        ELSE                                                              
021443           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
021444                                   MOD-IDSTATNR-IN-ATTR(STAT-IX)          
021445           MOVE MFS-ROER-EJ-FAELT TO MOD-IDSTATNR-IN(STAT-IX)             
021446        END-IF                                                            
021447        ADD +1 TO STAT-IX                                                 
021448     END-PERFORM                                                          
021450     .                                                                    
021500     EJECT                                                                
021700 F-LAES-VISA-INFO SECTION.                                                
021800                                                                          
021810     PERFORM IMS-GET-ARTC01                                               
021820     IF SEGMENT-FINNS                                                     
021830        IF ART-KDERS-UTG = ZERO                                           
021900           PERFORM FA-LAES-GRUNDDATA                                      
021910        ELSE                                                              
021911           MOVE ARTIKEL-UTGANGEN TO MED-IDMFSFEL                          
021912           CALL WMEDKONV USING MED-WMEDAREA                               
021913           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
021914           PERFORM MFS-RENSA-FAELT-UT                                     
021915           PERFORM MFS-RENSA-IDSTATNR-UT                                  
021920        END-IF                                                            
022000     ELSE                                                                 
022200        MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                               
022300        CALL WMEDKONV USING MED-WMEDAREA                                  
022400        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
022500        PERFORM MFS-RENSA-FAELT-UT                                        
022510        PERFORM MFS-RENSA-IDSTATNR-UT                                     
022600     END-IF                                                               
022900     .                                                                    
023000     EJECT                                                                
023100 FA-LAES-GRUNDDATA SECTION.                                               
023200                                                                          
023500     MOVE ART-KDPRODSL TO MOD-KDPRODSL                                    
023600     MOVE ART-IDFKNGRP TO MOD-IDFKNGRP                                    
023601                                                                          
023610     PERFORM IMS-GET-ARTC11                                               
023612     IF SEGMENT-FINNS                                                     
023613        MOVE +1 TO STAT-IX                                                
023614        PERFORM UNTIL STAT-IX > STAT-IX-MAX                               
023615           MOVE CLAG-IDSTATNR(STAT-IX)                                    
023616                              TO MOD-IDSTATNR-UT(STAT-IX)                 
023617           ADD +1 TO STAT-IX                                              
023618        END-PERFORM                                                       
023627     ELSE                                                                 
023628        PERFORM MFS-RENSA-IDSTATNR-UT                                     
023633     END-IF                                                               
023634                                                                          
023635     PERFORM IMS-GET-BENA11                                               
023636     IF SEGMENT-FINNS                                                     
023640        MOVE BENA11-TEXT-BEART TO MOD-BEART                               
023650     ELSE                                                                 
023660        MOVE MFS-RENSA-FAELT   TO MOD-BEART                               
023661     END-IF                                                               
023662                                                                          
023663     PERFORM IMS-GET-ARTG01                                               
023664     IF SEGMENT-FINNS                                                     
023665        MOVE ARTG01-ART-IDARTNR-MOTSV TO MOD-IDARTNR-MOTSV                
023666     ELSE                                                                 
023667        MOVE MFS-RENSA-FAELT          TO MOD-IDARTNR-MOTSV                
023668     END-IF                                                               
023700     .                                                                    
023801     EJECT                                                                
024100 G-KOLLA-INPUT SECTION.                                                   
024101******************************************************************        
024102* KONTROLL UPPDATERING AV IDSTATNR                               *        
024103******************************************************************        
024104                                                                          
024105     MOVE JA TO INDATA-SW                                                 
024106                                                                          
024107     IF MID-INPUT = ALL '+'                                               
024108        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSINF                         
024109        CALL WMEDKONV USING MED-WMEDAREA                                  
024110        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
024111        PERFORM MFS-ROER-EJ-FAELT-IN                                      
024113        MOVE NEJ TO INDATA-SW                                             
024114     ELSE                                                                 
024115        PERFORM IMS-GET-ARTC01                                            
024116        IF SEGMENT-FINNS                                                  
024117           IF ART-KDERS-UTG = ZERO                                        
024118              PERFORM GA-KOLLA-IDSTATNR                                   
024120              IF INDATA-FEL                                               
024121                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSINF                
024122                 CALL WMEDKONV USING MED-WMEDAREA                         
024123                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
024124                                    MSG-KOM-IDMFSMED                      
024125                 PERFORM MFS-ROER-EJ-FAELT-IN                             
024126              END-IF                                                      
024127           ELSE                                                           
024128              MOVE ARTIKEL-UTGANGEN TO MED-IDMFSFEL                       
024129              CALL WMEDKONV USING MED-WMEDAREA                            
024130              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
024131                                 MSG-KOM-IDMFSMED                         
024132              PERFORM MFS-RENSA-FAELT-UT                                  
024133              PERFORM MFS-RENSA-IDSTATNR-UT                               
024134              PERFORM MFS-RENSA-FAELT-IN                                  
024135              MOVE NEJ TO INDATA-SW                                       
024136           END-IF                                                         
024139        ELSE                                                              
024140           MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                            
024141           CALL WMEDKONV USING MED-WMEDAREA                               
024142           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
024143                              MSG-KOM-IDMFSMED                            
024144           PERFORM MFS-RENSA-FAELT-UT                                     
024145           PERFORM MFS-RENSA-IDSTATNR-UT                                  
024146           PERFORM MFS-RENSA-FAELT-IN                                     
024147           MOVE NEJ TO INDATA-SW                                          
024148        END-IF                                                            
024150     END-IF                                                               
024151     .                                                                    
024152     EJECT                                                                
024153 GA-KOLLA-IDSTATNR SECTION.                                               
024154                                                                          
024155     MOVE +1 TO STAT-IX                                                   
024156     PERFORM UNTIL STAT-IX > STAT-IX-MAX                                  
024157        IF MID-IDSTATNR(STAT-IX) = ALL '+'                                
024158           CONTINUE                                                       
024159        ELSE                                                              
024160           IF MID-IDSTATNR(STAT-IX) NUMERIC                               
024161              MOVE MFS-NUM-FAELT-RAETT                                    
024162                              TO MOD-IDSTATNR-IN-ATTR(STAT-IX)            
024164           ELSE                                                           
024165              MOVE MFS-NUM-FAELT-FEL                                      
024166                              TO MOD-IDSTATNR-IN-ATTR(STAT-IX)            
024167              MOVE NEJ TO INDATA-SW                                       
024168           END-IF                                                         
024169        END-IF                                                            
024170        ADD +1 TO STAT-IX                                                 
024171     END-PERFORM                                                          
024172     .                                                                    
024173     EJECT                                                                
024180 H-UPPDATERA SECTION.                                                     
024181                                                                          
024188     PERFORM IMS-GET-ARTC01                                               
024189     PERFORM IMS-GET-ARTC11                                               
024190     IF SEGMENT-FINNS                                                     
024191        PERFORM HA-UPPDATERA-IDSTATNR                                     
024192                                                                          
024193        MOVE INF-UPDATE-DONE TO MED-IDMFSINF                              
024194        CALL WMEDKONV USING MED-WMEDAREA                                  
024195        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
024196                           MSG-KOM-IDMFSMED                               
024197        PERFORM MFS-FORM-ATTR                                             
024198        PERFORM MFS-RENSA-FAELT-IN                                        
024199     END-IF                                                               
024200     .                                                                    
024201     EJECT                                                                
024202 HA-UPPDATERA-IDSTATNR SECTION.                                           
024203******************************************************************        
024205* KONTROLL OM SVENSKT STATNUMMER(5) UPPDATERAS                   *        
024206******************************************************************        
024207     MOVE +1 TO STAT-IX                                                   
024208     PERFORM UNTIL STAT-IX > STAT-IX-MAX                                  
024209        IF MID-IDSTATNR(STAT-IX) = ALL '+'                                
024210           CONTINUE                                                       
024211        ELSE                                                              
024212           MOVE MID-IDSTATNR(STAT-IX) TO WS-IDSTATNR                      
024213           MOVE WS-IDSTATNR           TO CLAG-IDSTATNR(STAT-IX)           
024215           MOVE MFS-ADD-LYS-UPP-FAELT                                     
024216                            TO MOD-IDSTATNR-UT-ATTR(STAT-IX)              
024220        END-IF                                                            
024221        ADD +1 TO STAT-IX                                                 
024222     END-PERFORM                                                          
024224     PERFORM IMS-REPL-ARTC                                                
024225     .                                                                    
024226     EJECT                                                                
024228 HAA-KOLLA-GB-BELGIEN SECTION.                                            
024229******************************************************************        
024230*   ENGELSKT STATNR(2) / BELGISKT STATNR(3) UPPDATERAS OM        *        
024231*   INGET STATNR FINNS REGISTRERAT.                              *        
024232******************************************************************        
024233     MOVE ZERO TO WS-IDSTATNR-GB                                          
024234                  WS-IDSTATNR-BELGIEN                                     
024235                                                                          
024236     IF CLAG-IDSTATNR(2) = ZERO                                           
024237        MOVE WS-IDSTATNR-POS3-8 TO WS-IDSTATNR-GB                         
024238        MOVE WS-IDSTATNR-GB     TO CLAG-IDSTATNR(2)                       
024240        MOVE MFS-ADD-LYS-UPP-FAELT                                        
024241                            TO MOD-IDSTATNR-UT-ATTR(2)                    
024242     END-IF                                                               
024243                                                                          
024244     IF CLAG-IDSTATNR(3) = ZERO                                           
024246        MOVE WS-IDSTATNR-POS3-9  TO WS-BELGIEN-POS2-8                     
024247        MOVE ZERO                TO WS-BELGIEN-POS1                       
024248                                    WS-BELGIEN-POS9                       
024249        MOVE WS-IDSTATNR-BELGIEN TO CLAG-IDSTATNR(3)                      
024251        MOVE MFS-ADD-LYS-UPP-FAELT                                        
024252                            TO MOD-IDSTATNR-UT-ATTR(3)                    
024253     END-IF                                                               
024254     .                                                                    
024255     EJECT                                                                
024260 MFS-RENSA-FAELT-UT SECTION.                                              
024300                                                                          
024500     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
024600                             MOD-KDPRODSL                                 
024701                             MOD-IDFKNGRP                                 
024710                             MOD-IDARTNR-MOTSV                            
024800     .                                                                    
024901     SKIP2                                                                
024903 MFS-RENSA-IDSTATNR-UT SECTION.                                           
024904                                                                          
024907     MOVE +1 TO STAT-IX                                                   
024908     PERFORM UNTIL STAT-IX > STAT-IX-MAX                                  
024909        MOVE MFS-RENSA-FAELT TO MOD-IDSTATNR-UT(STAT-IX)                  
024910        ADD +1 TO STAT-IX                                                 
024911     END-PERFORM                                                          
024920     .                                                                    
025103     EJECT                                                                
025110 MFS-RENSA-FAELT-IN SECTION.                                              
025120                                                                          
025200     MOVE +1 TO STAT-IX                                                   
025300     PERFORM UNTIL STAT-IX > STAT-IX-MAX                                  
025310        MOVE MFS-RENSA-FAELT TO MOD-IDSTATNR-IN(STAT-IX)                  
025320        ADD +1 TO STAT-IX                                                 
025330     END-PERFORM                                                          
025600     .                                                                    
025700     SKIP2                                                                
025710 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
025720                                                                          
025730     MOVE +1 TO STAT-IX                                                   
025740     PERFORM UNTIL STAT-IX > STAT-IX-MAX                                  
025750       MOVE MFS-ROER-EJ-FAELT TO MOD-IDSTATNR-IN(STAT-IX)                 
025760       ADD +1 TO STAT-IX                                                  
025770     END-PERFORM                                                          
025780     .                                                                    
025790     EJECT                                                                
027500 MFS-FORM-ATTR SECTION.                                                   
027600                                                                          
028010     MOVE +1 TO STAT-IX                                                   
028020     PERFORM UNTIL STAT-IX > STAT-IX-MAX                                  
028030       MOVE MFS-FORMATETS-ATTR                                            
028031                   TO MOD-IDSTATNR-IN-ATTR(STAT-IX)                       
028040       ADD +1 TO STAT-IX                                                  
028050     END-PERFORM                                                          
028060     .                                                                    
028800     EJECT                                                                
028900* --- IMS SEKTIONER ---                                                   
029000     SKIP3                                                                
029100 IMS-GET-MSG SECTION.                                                     
029300     MOVE '  QC' TO GODK-STATUSKODER                                      
029400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
029500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029600     PERFORM IMS-STATUSKONTROLL                                           
029700     .                                                                    
029800     SKIP3                                                                
029900 IMS-INSERT-MSG SECTION.                                                  
030100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
030200       MOVE '0' TO MFS-KDHUVOMR                                           
030300     END-IF                                                               
030400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
030500     MOVE SPACE TO GODK-STATUSKODER                                       
030600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
030700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030800     PERFORM IMS-STATUSKONTROLL                                           
030900     .                                                                    
031001     EJECT                                                                
031002 IMS-GET-WMSGKOM SECTION.                                                 
031003     MOVE '  QD' TO GODK-STATUSKODER                                      
031004     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
031005     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031006     PERFORM IMS-STATUSKONTROLL                                           
031007     .                                                                    
031008     SKIP3                                                                
031009 IMS-INSERT-WMSGKOM SECTION.                                              
031014     MOVE '  ' TO GODK-STATUSKODER                                        
031015     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
031016     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
031017     PERFORM IMS-STATUSKONTROLL                                           
031018     .                                                                    
031019     EJECT                                                                
031020 IMS-GET-ARTC01 SECTION.                                                  
031021     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031022          DELIMITED BY SIZE INTO SSA1                                     
031023     MOVE '  GE' TO GODK-STATUSKODER                                      
031024     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-2 SSA1                    
031025     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031026     PERFORM IMS-STATUSKONTROLL                                           
031027     .                                                                    
031028     SKIP3                                                                
031044 IMS-GET-ARTC11 SECTION.                                                  
031047     MOVE 'WLARTC11 ' TO SSA1                                             
031048     MOVE '  GE' TO GODK-STATUSKODER                                      
031049     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-2 SSA1                  
031050     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031051     PERFORM IMS-STATUSKONTROLL                                           
031052     .                                                                    
031053     SKIP3                                                                
031054 IMS-REPL-ARTC SECTION.                                                   
031055     MOVE '  ' TO GODK-STATUSKODER                                        
031056     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-2                       
031057     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031058     PERFORM IMS-STATUSKONTROLL                                           
031059     .                                                                    
031060     EJECT                                                                
031061 IMS-GET-ARTG01 SECTION.                                                  
031062     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
031063          DELIMITED BY SIZE INTO SSA1                                     
031064     MOVE '  GE' TO GODK-STATUSKODER                                      
031065     CALL CBLTDLI USING GU ARTG-PCB DLI-IO-AREA SSA1                      
031066     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
031067     PERFORM IMS-STATUSKONTROLL                                           
031068     .                                                                    
031069     SKIP3                                                                
031070 IMS-GET-BENA11 SECTION.                                                  
031071     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
031072          DELIMITED BY SIZE INTO SSA1                                     
031073     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
031074          DELIMITED BY SIZE INTO SSA2                                     
031075     MOVE '  GE' TO GODK-STATUSKODER                                      
031076     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
031077     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
031078     PERFORM IMS-STATUSKONTROLL                                           
031079     .                                                                    
031080     SKIP3                                                                
031200 IMS-STATUSKONTROLL SECTION.                                              
031400     SET STATUS-IX TO 1                                                   
031500     SEARCH GODK-STATUS                                                   
031600       AT END                                                             
031700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
031800         DELIMITED BY SIZE INTO FELTEXT                                   
031900         CALL FELLOG                                                      
032000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032100         CONTINUE                                                         
032200     END-SEARCH                                                           
032300     .                                                                    
