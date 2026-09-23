001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W1011800.                                                
001300*AUTHOR.         BODIL LINDAHL.                                           
001400*DATE-WRITTEN.   93/11/16.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        REGISTRERING OCH UPPDATERING AV ARTIKLAR KEMI                    
002000*        - ARTIKLAR MED KDFARLIG 4 ELLER 6.                               
002200*                                                                         
002301*        PROGRAMMET UPPDATERAR WLARTN (WDD5)                              
002302*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
002304*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002305*        PROGRAMMET LÄSER      W6KVAH (W6D2)                              
002310*        PROGRAMMET LÄSER      W6KVAG (W6H7B)                             
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W1T118                                              
002610*                     W1T118U                                             
002700*        MID:         W1I11801                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W1O11801                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W1011800'.            
004000 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004100 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004110 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +517 COMP SYNC.        
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004920 77  ARTIKEL-SW                  PIC X       VALUE SPACE.                 
004930 77  WS-TENOTE1-IFYLLT           PIC X       VALUE SPACE.                 
004940 77  WS-TENOTE2-IFYLLT           PIC X       VALUE SPACE.                 
004950 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005210 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
005212 77  WS-KVFLAMP                  PIC S9(3) COMP-3.                        
005213 77  WS-KVFLAMP-NEG              PIC S9(3) COMP-3.                        
005214 77  WS-KVNTOFG                  PIC S9(2)V9(3) COMP-3.                   
005215 77  WS-KVVOC                    PIC S9(2)V9(3) COMP-3.                   
005216 77  WS-REBLYPRO                 PIC S9(2)V9(3) COMP-3.                   
005217 77  WS-SUEQFG                   PIC S9(3)V9(4) COMP-3.                   
005218 77  WS-VKFORSFG                 PIC S9(4)V9(3) COMP-3.                   
005219 77  WS-VLFG                     PIC S9(4)V9(3) COMP-3.                   
005220 77  WS-VKART-FG                 PIC S9(7) COMP-3.                        
005221 77  WS-IDARTNR-RECEPT           PIC 9(9)    VALUE ZERO.                  
005222 77  WS-KDFGPRIO                 PIC 9(3)    VALUE ZERO.                  
005223 77  WS-IDANMNR                  PIC 9(6)    VALUE ZERO.                  
005224 77  WS-REKSIFFR                 PIC 9(1)    VALUE ZERO.                  
005230                                                                          
005401 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005402     88  INDATA-OK                           VALUE 'J'.                   
005410     88  INDATA-FEL                          VALUE 'N'.                   
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900                                                                          
006000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006100     88  EGEN-MID                            VALUE '1118'.                
006700     88  HELP-MID                            VALUE '0551'.                
006710                                                                          
006733 01  WS-KDARTHNT                 PIC 9(6).                                
006734 01  FILLER REDEFINES WS-KDARTHNT.                                        
006740     03  WS-KDARTHNT-V           PIC 9(3).                                
006750     03  WS-KDARTHNT-H           PIC 9(3).                                
006800     EJECT                                                                
006900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007000 01  GENERELLA-SUBPROGRAM.                                                
007100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007410     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007420     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007500     EJECT                                                                
007600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007700*01 -COPY WMEDAREA                                                        
007800     SKIP3                                                                
007810*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
007820*01 -COPY WMSGINIT                                                        
007830     SKIP3                                                                
007900 01  MESSAGE-CODES.                                                       
008001     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008002     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008003     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008010     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008020     03  ARTIKEL-SAKNAS          PIC X(3)    VALUE '017'.                 
008200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008301     EJECT                                                                
008310*01  -COPY WDECAREA                                                       
008400     EJECT                                                                
008500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008600*                                                                         
008700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008800     SKIP3                                                                
008900*01  MID -COPY W1I11801                                                   
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009200     SKIP3                                                                
009300*01  -COPY WMSGAREA                                                       
009400     EJECT                                                                
009500     03  MOD REDEFINES MSG-AREA.                                          
009600*      05  -COPY W1O11801                                                 
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009900     SKIP3                                                                
010000*01  -COPY WMFSAREA                                                       
010100     EJECT                                                                
010200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010400                                                                          
010500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010600                                                                          
010700 01  NYCKLAR-TILL-DLI.                                                    
010801     03  W-IDARTNR-X.                                                     
010802         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010815     03  W-IDSKYLT-X.                                                     
010816         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
010841     03  W-W6H7B1KY-MAX-X.                                                
010842         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
010843         05  W-DAREGDAT-9KOMPL-MAX                                        
010844                                 PIC 9(8)    VALUE ZERO.                  
010845         05  W-IDLEVNR-MAX       PIC X(5)    VALUE SPACE.                 
010850         05  W-KVKRKNTR-MAX      PIC S9(1)   VALUE ZERO COMP-3.           
010860         05  W-IDKR-MAX          PIC 9(5)    VALUE ZERO.                  
010870     03  W-W6H7B1KY-MIN-X.                                                
010880         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
010881         05  W-DAREGDAT-9KOMPL-MIN                                        
010882                                 PIC 9(8)    VALUE ZERO.                  
010890         05  W-IDLEVNR-MIN       PIC X(5)    VALUE SPACE.                 
010891         05  W-KVKRKNTR-MIN      PIC S9(1)   VALUE ZERO COMP-3.           
010892         05  W-IDKR-MIN          PIC 9(5)    VALUE ZERO.                  
010900     SKIP2                                                                
011000*    --- STATUS-KOD FRÅN IMS                                              
011100 01  STATUS-WS                   PIC XX.                                  
011200     88  SEGMENT-FINNS                       VALUE '  '.                  
011400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011500     SKIP2                                                                
011600 01  GODK-STATUSKODER.                                                    
011700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011800                                                                          
011900 01  SSA1                        PIC X(96).                               
012000 01  SSA2                        PIC X(64).                               
012100     EJECT                                                                
012200*    --- IMS FUNKTIONSKODER                                               
012300*01  -COPY W0003                                                          
012500     EJECT                                                                
012600*    ---  DLI INPUT-OUTPUT AREA                                           
012700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012800     SKIP3                                                                
012900 01  DLI-IO-AREA.                                                         
013000     03  IO-AREA                 PIC X(1200)  VALUE SPACE.                
013101     SKIP3                                                                
013105     03  WLARTC01 REDEFINES IO-AREA.                                      
013106*        05  -COPY WDK601                                                 
013107     EJECT                                                                
013108     03  WLARTC11 REDEFINES IO-AREA.                                      
013109*        05  -COPY WDK611                                                 
013110     EJECT                                                                
013120     03  WLBENA11 REDEFINES IO-AREA.                                      
013121*        05  -COPY WDD311  -PRE BENA11-                                   
013122     EJECT                                                                
013123 01  DLI-IO-AREA2.                                                        
013124     03  IO-AREA2                PIC X(200)  VALUE SPACE.                 
013125     SKIP3                                                                
013126     03  WLARTN01 REDEFINES IO-AREA2.                                     
013127*        05  -COPY WDD501  -PRE ARTN01-                                   
013128     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600                                                                          
013700*01  -COPY W0009  -PRE MSG-                                               
013801     EJECT                                                                
013802*01  -COPY W0008  -PRE USEA-                                              
013803     05  FILLER                  PIC X.                                   
013804     EJECT                                                                
013805*01  -COPY W0008  -PRE ARTN-                                              
013806     05  FILLER                  PIC X.                                   
013807     EJECT                                                                
013808*01  -COPY W0008  -PRE ARTC-                                              
013809     05  FILLER                  PIC X.                                   
013810     EJECT                                                                
013811*01  -COPY W0008  -PRE BENA-                                              
013812     05  FILLER                  PIC X.                                   
013813     EJECT                                                                
013814*01  -COPY W0008  -PRE KVAH-                                              
013815     05  FILLER                  PIC X.                                   
013816     EJECT                                                                
013817*01  -COPY W0008  -PRE KVAG-                                              
013820     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
014002                                   ARTN-PCB ARTC-PCB                      
014003     BENA-PCB KVAH-PCB KVAG-PCB.                                          
014004     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
014005                                   ARTN-PCB ARTC-PCB                      
014010     BENA-PCB KVAH-PCB KVAG-PCB.                                          
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014801         IF MFS-UPDATE                                                    
014803           PERFORM G-KOLLA-INPUT                                          
014804           IF INDATA-OK                                                   
014805             PERFORM H-UPPDATERA                                          
014806           END-IF                                                         
014807         ELSE                                                             
014808           IF MFS-FIRST                                                   
014809              PERFORM C-FOERSTA-SIDA                                      
014810           ELSE                                                           
014811              PERFORM E-SAMMA-SIDA                                        
014812           END-IF                                                         
014820         END-IF                                                           
015200         PERFORM F-LAES-VISA-INFO                                         
015300       END-IF                                                             
015400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
015500       PERFORM IMS-INSERT-MSG                                             
015600     END-IF                                                               
015800                                                                          
015900     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400                                                                          
016500     IF MSG-DUBBLA-TRANSKODER                                             
016600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I11801-CTX             
016700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
016800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
016900     ELSE                                                                 
017000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I11801-CTX              
017100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017300     END-IF                                                               
017400                                                                          
017500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
017800                                                                          
017900     MOVE LOW-VALUE TO MSG-AREA                                           
018000     MOVE 'W1O118N1' TO MFS-IDMOD                                         
018100     MOVE '1118' TO MOD-IDTRANS                                           
018200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018300                                                                          
018400     IF EGEN-MID OR HELP-MID                                              
018500       CONTINUE                                                           
018600     ELSE                                                                 
018700       MOVE SPACE TO MFS-KDTRTYP                                          
018800       MOVE '7' TO MFS-IDPFK                                              
018900     END-IF                                                               
019800                                                                          
019900     ACCEPT DAGENS-DATUM FROM DATE                                        
020000     .                                                                    
020100     EJECT                                                                
020200 B-KOLLA-NYCKLAR SECTION.                                                 
020300                                                                          
020400     MOVE JA TO NYCKLAR-SW                                                
020500     MOVE LOW-VALUE  TO  W-W6H7B1KY-MIN-X                                 
020501     MOVE HIGH-VALUE TO  W-W6H7B1KY-MAX-X                                 
020503     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
020504                             MOD-IDPSN-DOLT                               
020505                                                                          
020506     MOVE ALL '+' TO MSGI-WMSGINIT                                        
020507     MOVE '001'             TO MSGI-KDCALL                                
020508     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020509     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020510     MOVE '1118'            TO MSGI-IDTRANS                               
020511     IF MFS-IDTRANS = '1118'                                              
020512     OR (MID-IDARTNR-IN NUMERIC                                           
020513     AND MID-IDARTNR-IN > ZERO)                                           
020514         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
020515     END-IF                                                               
020516     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020517     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
020518     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
020519                                                                          
020520     IF MID-IDARTNR-IN = ALL '+'                                          
020521       CONTINUE                                                           
020522     ELSE                                                                 
020523       MOVE '7'         TO MFS-IDPFK                                      
020524       MOVE SPACE       TO MFS-KDTRTYP                                    
020525     END-IF                                                               
020526                                                                          
020527     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
020528       MOVE +1 TO SPRAK-IX                                                
020529       MOVE 'S  ' TO MED-IDSKYLT                                          
020530     ELSE                                                                 
020531       MOVE +2 TO SPRAK-IX                                                
020532       MOVE 'GB ' TO MED-IDSKYLT                                          
020533     END-IF                                                               
020534                                                                          
020535     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
020536       MOVE WS-IDARTNR TO W-IDARTNR                                       
020537                          W-IDARTNR-MAX                                   
020538                          W-IDARTNR-MIN                                   
020539     ELSE                                                                 
020540       MOVE NEJ TO NYCKLAR-SW                                             
020550     END-IF                                                               
020601                                                                          
020602     IF NYCKLAR-OK                                                        
020603       MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
020604       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
020605     ELSE                                                                 
020606       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
020610     END-IF                                                               
020700                                                                          
020800     IF NYCKLAR-FEL                                                       
020900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021000       CALL WMEDKONV USING MED-WMEDAREA                                   
021100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021200       PERFORM MFS-RENSA-FAELT-IN                                         
021300       PERFORM MFS-RENSA-FAELT-UT                                         
021310       MOVE MFS-RENSA-FAELT TO MOD-TENOTE(1)                              
021320                               MOD-TENOTE(2)                              
021400     END-IF                                                               
021500     .                                                                    
021700     EJECT                                                                
021801 C-FOERSTA-SIDA SECTION.                                                  
021802                                                                          
021803     PERFORM MFS-RENSA-FAELT-IN                                           
021804     MOVE MFS-RENSA-FAELT TO MOD-TENOTE(1)                                
021805                             MOD-TENOTE(2)                                
021806     .                                                                    
021807     EJECT                                                                
021808 E-SAMMA-SIDA SECTION.                                                    
021809                                                                          
021810     IF EGEN-MID OR HELP-MID                                              
021811       IF MID-INPUT = ALL '+' AND MID-FLBORT = ALL '+'                    
021812         PERFORM MFS-RENSA-FAELT-IN                                       
021813         MOVE MFS-RENSA-FAELT TO MOD-TENOTE(1)                            
021814                                 MOD-TENOTE(2)                            
021816       ELSE                                                               
021817         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
021818         CALL WMEDKONV USING MED-WMEDAREA                                 
021819         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
021820         PERFORM EA-MID-INDATA-TILL-MOD                                   
021821       END-IF                                                             
021822     ELSE                                                                 
021823       PERFORM MFS-RENSA-FAELT-IN                                         
021824       MOVE MFS-RENSA-FAELT TO MOD-TENOTE(1)                              
021825                               MOD-TENOTE(2)                              
021827     END-IF                                                               
021828     .                                                                    
021829     EJECT                                                                
021830 EA-MID-INDATA-TILL-MOD SECTION.                                          
021831                                                                          
021832     MOVE NEJ TO WS-TENOTE1-IFYLLT                                        
021833                 WS-TENOTE2-IFYLLT                                        
021834                                                                          
021835     IF MID-FLBORT = ALL '+'                                              
021836        MOVE MFS-RENSA-FAELT TO MOD-FLBORT                                
021840     ELSE                                                                 
021841        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLBORT-ATTR                     
021842        MOVE MFS-ROER-EJ-FAELT TO MOD-FLBORT                              
021843     END-IF                                                               
021844                                                                          
021845     IF MID-KDFARG = ALL '+'                                              
021846        MOVE MFS-RENSA-FAELT TO MOD-KDFARG-IN                             
021847     ELSE                                                                 
021848        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFARG-ATTR                     
021849        MOVE MFS-ROER-EJ-FAELT TO MOD-KDFARG-IN                           
021850     END-IF                                                               
021855                                                                          
021856     IF MID-KDLACK = ALL '+'                                              
021857        MOVE MFS-RENSA-FAELT TO MOD-KDLACK-IN                             
021858     ELSE                                                                 
021859        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDLACK-ATTR                     
021860        MOVE MFS-ROER-EJ-FAELT TO MOD-KDLACK-IN                           
021861     END-IF                                                               
021862                                                                          
021863     IF MID-IDARTNR-RECEPT = ALL '+'                                      
021864        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-RECEPT-IN                     
021865     ELSE                                                                 
021866        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-RECEPT-ATTR             
021867        MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-RECEPT-IN                   
021868     END-IF                                                               
021879                                                                          
021880     IF MID-VKFORSFG = ALL '+'                                            
021881        MOVE MFS-RENSA-FAELT TO MOD-VKFORSFG-IN                           
021883     ELSE                                                                 
021884        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VKFORSFG-ATTR                   
021885        MOVE MFS-ROER-EJ-FAELT TO MOD-VKFORSFG-IN                         
021886     END-IF                                                               
021896                                                                          
021897     IF MID-FLVARINF = ALL '+'                                            
021898        MOVE MFS-RENSA-FAELT TO MOD-FLVARINF-IN                           
021899     ELSE                                                                 
021900        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLVARINF-ATTR                   
021901        MOVE MFS-ROER-EJ-FAELT TO MOD-FLVARINF-IN                         
021902     END-IF                                                               
021906                                                                          
021907     IF MID-FLVARINF-SDS = ALL '+'                                        
021908        MOVE MFS-RENSA-FAELT TO MOD-FLVARINF-SDS-IN                       
021909     ELSE                                                                 
021910        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLVARINF-SDS-ATTR               
021911        MOVE MFS-ROER-EJ-FAELT TO MOD-FLVARINF-SDS-IN                     
021912     END-IF                                                               
021916                                                                          
021917     IF MID-IDVARINF = ALL '+'                                            
021918        MOVE MFS-RENSA-FAELT TO MOD-IDVARINF-IN                           
021919     ELSE                                                                 
021920        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDVARINF-ATTR                   
021921        MOVE MFS-ROER-EJ-FAELT TO MOD-IDVARINF-IN                         
021922     END-IF                                                               
021923                                                                          
021924     IF MID-IDVARINF-SDS = ALL '+'                                        
021925        MOVE MFS-RENSA-FAELT TO MOD-IDVARINF-SDS-IN                       
021926     ELSE                                                                 
021927        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDVARINF-SDS-ATTR               
021928        MOVE MFS-ROER-EJ-FAELT TO MOD-IDVARINF-SDS-IN                     
021929     END-IF                                                               
021930                                                                          
021931     IF MID-KVNTOFG = ALL '+'                                             
021932        MOVE MFS-RENSA-FAELT TO MOD-KVNTOFG-IN                            
021933     ELSE                                                                 
021934        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVNTOFG-ATTR                    
021935        MOVE MFS-ROER-EJ-FAELT TO MOD-KVNTOFG-IN                          
021936     END-IF                                                               
021947                                                                          
021948     IF MID-KDSORT-KVNTOFG = ALL '+'                                      
021949        MOVE MFS-RENSA-FAELT TO MOD-KDSORT-KVNTOFG-IN                     
021950     ELSE                                                                 
021951        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSORT-KVNTOFG-ATTR             
021952        MOVE MFS-ROER-EJ-FAELT TO MOD-KDSORT-KVNTOFG-IN                   
021953     END-IF                                                               
021958                                                                          
021959     IF MID-KVVOC = ALL '+'                                               
021960        MOVE MFS-RENSA-FAELT TO MOD-KVVOC-IN                              
021970     ELSE                                                                 
021971        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVOC-ATTR                      
021972        MOVE MFS-ROER-EJ-FAELT TO MOD-KVVOC-IN                            
021973     END-IF                                                               
021975                                                                          
021976     IF MID-SUEQFG = ALL '+'                                              
021977        MOVE MFS-RENSA-FAELT TO MOD-SUEQFG-IN                             
021979     ELSE                                                                 
021980        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUEQFG-ATTR                     
021990        MOVE MFS-ROER-EJ-FAELT TO MOD-SUEQFG-IN                           
021991     END-IF                                                               
021992                                                                          
021993     IF MID-VLFG = ALL '+'                                                
021994        MOVE MFS-RENSA-FAELT TO MOD-VLFG-IN                               
021995     ELSE                                                                 
021996        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VLFG-ATTR                       
021997        MOVE MFS-ROER-EJ-FAELT TO MOD-VLFG-IN                             
021998     END-IF                                                               
022009                                                                          
022010     IF MID-KDSORT-VLFG = ALL '+'                                         
022011        MOVE MFS-RENSA-FAELT TO MOD-KDSORT-VLFG-IN                        
022012     ELSE                                                                 
022013        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSORT-VLFG-ATTR                
022014        MOVE MFS-ROER-EJ-FAELT TO MOD-KDSORT-VLFG-IN                      
022015     END-IF                                                               
022016                                                                          
022021     IF MID-FLNEG = ALL '+'                                               
022022        MOVE MFS-RENSA-FAELT TO MOD-FLNEG-IN                              
022023     ELSE                                                                 
022024        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLNEG-ATTR                      
022025        MOVE MFS-ROER-EJ-FAELT TO MOD-FLNEG-IN                            
022026     END-IF                                                               
022027                                                                          
022028     IF MID-KVFLAMP = ALL '+'                                             
022029        MOVE MFS-RENSA-FAELT TO MOD-KVFLAMP-IN                            
022030     ELSE                                                                 
022031        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVFLAMP-ATTR                    
022032        MOVE MFS-ROER-EJ-FAELT TO MOD-KVFLAMP-IN                          
022033     END-IF                                                               
022037                                                                          
022038     IF MID-FLFROST = ALL '+'                                             
022039        MOVE MFS-RENSA-FAELT TO MOD-FLFROST-IN                            
022041     ELSE                                                                 
022042        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFROST-ATTR                    
022043        MOVE MFS-ROER-EJ-FAELT TO MOD-FLFROST-IN                          
022044     END-IF                                                               
022047                                                                          
022048     IF MID-FLTACTIL = ALL '+'                                            
022049        MOVE MFS-RENSA-FAELT TO MOD-FLTACTIL-IN                           
022050     ELSE                                                                 
022051        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTACTIL-ATTR                   
022052        MOVE MFS-ROER-EJ-FAELT TO MOD-FLTACTIL-IN                         
022053     END-IF                                                               
022057                                                                          
022058     IF MID-IDPSN = ALL '+'                                               
022059          MOVE MFS-RENSA-FAELT TO MOD-IDPSN-IN                            
022060     ELSE                                                                 
022061        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPSN-ATTR                      
022062        MOVE MFS-ROER-EJ-FAELT TO MOD-IDPSN-IN                            
022063     END-IF                                                               
022068                                                                          
022069     IF MID-IDAO-FG = ALL '+'                                             
022070        MOVE MFS-RENSA-FAELT TO MOD-IDAO-FG-IN                            
022071     ELSE                                                                 
022072        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDAO-FG-ATTR                    
022073        MOVE MFS-ROER-EJ-FAELT TO MOD-IDAO-FG-IN                          
022074     END-IF                                                               
022075                                                                          
022076     IF MID-KDFGPRIO = ALL '+'                                            
022077        MOVE MFS-RENSA-FAELT TO MOD-KDFGPRIO-IN                           
022078     ELSE                                                                 
022079        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDFGPRIO-ATTR                   
022080        MOVE MFS-ROER-EJ-FAELT TO MOD-KDFGPRIO-IN                         
022081     END-IF                                                               
022085                                                                          
022086     IF MID-BEEMBMAT = ALL '+'                                            
022087        MOVE MFS-RENSA-FAELT TO MOD-BEEMBMAT-IN                           
022088     ELSE                                                                 
022089        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BEEMBMAT-ATTR                   
022090        MOVE MFS-ROER-EJ-FAELT TO MOD-BEEMBMAT-IN                         
022091     END-IF                                                               
022092                                                                          
022093     IF MID-IDANMNR = ALL '+'                                             
022094        MOVE MFS-RENSA-FAELT TO MOD-IDANMNR-IN                            
022095     ELSE                                                                 
022096        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANMNR-ATTR                    
022097        MOVE MFS-ROER-EJ-FAELT TO MOD-IDANMNR-IN                          
022098     END-IF                                                               
022099                                                                          
022100     IF MID-REKSIFFR-ANMNR = ALL '+'                                      
022101        MOVE MFS-RENSA-FAELT TO MOD-REKSIFFR-IN                           
022102     ELSE                                                                 
022103        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REKSIFFR-ATTR                   
022104        MOVE MFS-ROER-EJ-FAELT TO MOD-REKSIFFR-IN                         
022105     END-IF                                                               
022106                                                                          
022107     IF MID-VKART-FG = ALL '+'                                            
022108        CONTINUE                                                          
022109     ELSE                                                                 
022110        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-VKART-FG-ATTR                   
022111        MOVE MFS-ROER-EJ-FAELT TO MOD-VKART-FG-IN                         
022112     END-IF                                                               
022113                                                                          
022114     IF MID-TENOTE(1)= ALL '+'                                            
022115        CONTINUE                                                          
022116     ELSE                                                                 
022117        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-ATTR(1)                  
022118        MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE(1)                           
022119        MOVE JA TO WS-TENOTE1-IFYLLT                                      
022120     END-IF                                                               
022121                                                                          
022122     IF MID-TENOTE(2)= ALL '+'                                            
022123        CONTINUE                                                          
022124     ELSE                                                                 
022125        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-ATTR(2)                  
022126        MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE(2)                           
022127        MOVE JA TO WS-TENOTE2-IFYLLT                                      
022128     END-IF                                                               
022129     .                                                                    
022130     EJECT                                                                
022131 F-LAES-VISA-INFO SECTION.                                                
022132                                                                          
022140     PERFORM FA-LAES-GRUNDDATA                                            
022200                                                                          
022300     IF SEGMENT-SAKNAS                                                    
022310        MOVE ARTIKEL-SAKNAS TO MED-IDMFSFEL                               
022500        CALL WMEDKONV USING MED-WMEDAREA                                  
022600        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023000     END-IF                                                               
023010                                                                          
023020     PERFORM FB-LAES-ARTIKELREG                                           
023100     .                                                                    
023200     EJECT                                                                
023300 FA-LAES-GRUNDDATA SECTION.                                               
023401                                                                          
023410     PERFORM IMS-GET-ARTN01                                               
023420     IF SEGMENT-FINNS                                                     
023430        MOVE ARTN01-ART-BEEMBMAT        TO MOD-BEEMBMAT-UT                
023440        MOVE ARTN01-ART-FLFROST         TO MOD-FLFROST-UT                 
023450        MOVE ARTN01-ART-FLTACTIL        TO MOD-FLTACTIL-UT                
023460        MOVE ARTN01-ART-FLVARINF        TO MOD-FLVARINF-UT                
023470        MOVE ARTN01-ART-FLVARINF-SDS    TO MOD-FLVARINF-SDS-UT            
023480        MOVE ARTN01-ART-IDAO-FG         TO MOD-IDAO-FG-UT                 
023490        MOVE ARTN01-ART-IDVARINF        TO MOD-IDVARINF-UT                
023491        MOVE ARTN01-ART-IDVARINF-SDS    TO MOD-IDVARINF-SDS-UT            
023492        MOVE ARTN01-ART-KDFARG          TO MOD-KDFARG-UT                  
023493        MOVE ARTN01-ART-KDFGPRIO        TO MOD-KDFGPRIO-UT                
023494        MOVE ARTN01-ART-KDLACK          TO MOD-KDLACK-UT                  
023500        MOVE ARTN01-ART-KDSORT-KVNTOFG  TO MOD-KDSORT-KVNTOFG-UT          
023510        MOVE ARTN01-ART-KDSORT-VLFG     TO MOD-KDSORT-VLFG-UT             
023520        MOVE ARTN01-ART-KVFLAMP         TO MOD-KVFLAMP-UT                 
023610        MOVE ARTN01-ART-KVNTOFG         TO MOD-KVNTOFG-UT                 
023630        MOVE ARTN01-ART-KVVOC           TO MOD-KVVOC-UT                   
023650        MOVE ARTN01-ART-IDARTNR-RECEPT  TO MOD-IDARTNR-RECEPT-UT          
023670        MOVE ARTN01-ART-SUEQFG          TO MOD-SUEQFG-UT                  
023690        MOVE ARTN01-ART-VKFORSFG        TO MOD-VKFORSFG-UT                
023692        MOVE ARTN01-ART-VLFG            TO MOD-VLFG-UT                    
023693        MOVE ARTN01-ART-VKART-FG        TO MOD-VKART-FG-UT                
023694        MOVE ARTN01-ART-IDANMNR         TO MOD-IDANMNR-UT                 
023695        IF ARTN01-ART-IDANMNR = ZERO                                      
023696           MOVE MFS-RENSA-FAELT          TO MOD-REKSIFFR-UT               
023697        ELSE                                                              
023698           MOVE ARTN01-ART-REKSIFFR-ANMNR TO MOD-REKSIFFR-UT              
023699        END-IF                                                            
023700        IF WS-TENOTE1-IFYLLT = JA                                         
023701           CONTINUE                                                       
023702        ELSE                                                              
023703           MOVE ARTN01-ART-TENOTE(1)    TO MOD-TENOTE(1)                  
023704        END-IF                                                            
023705        IF WS-TENOTE2-IFYLLT = JA                                         
023706           CONTINUE                                                       
023707        ELSE                                                              
023708           MOVE ARTN01-ART-TENOTE(2)    TO MOD-TENOTE(2)                  
023709        END-IF                                                            
023710     ELSE                                                                 
023800        MOVE MFS-RENSA-FAELT            TO MOD-BEEMBMAT-UT                
023810                                           MOD-FLFROST-UT                 
023820                                           MOD-FLTACTIL-UT                
023830                                           MOD-FLVARINF-UT                
023840                                           MOD-FLVARINF-SDS-UT            
023850                                           MOD-IDAO-FG-UT                 
023860                                           MOD-IDVARINF-UT                
023870                                           MOD-IDVARINF-SDS-UT            
023880                                           MOD-KDFARG-UT                  
023890                                           MOD-KDLACK-UT                  
023891                                           MOD-KDFGPRIO-UT                
023892                                           MOD-KDSORT-KVNTOFG-UT          
023893                                           MOD-KDSORT-VLFG-UT             
023894                                           MOD-KVFLAMP-UT                 
023895                                           MOD-KVNTOFG-UT                 
023896                                           MOD-KVVOC-UT                   
023897                                           MOD-IDARTNR-RECEPT-UT          
023898                                           MOD-SUEQFG-UT                  
023899                                           MOD-VKFORSFG-UT                
023900                                           MOD-VLFG-UT                    
023921        IF WS-TENOTE1-IFYLLT = JA                                         
023922           CONTINUE                                                       
023923        ELSE                                                              
023924           MOVE MFS-RENSA-FAELT         TO MOD-TENOTE(1)                  
023925        END-IF                                                            
023926        IF WS-TENOTE2-IFYLLT = JA                                         
023927           CONTINUE                                                       
023928        ELSE                                                              
023929           MOVE MFS-RENSA-FAELT         TO MOD-TENOTE(2)                  
023930        END-IF                                                            
023931                                                                          
023940     END-IF                                                               
023941     .                                                                    
023942     EJECT                                                                
023960 FB-LAES-ARTIKELREG SECTION.                                              
023970                                                                          
024060                                                                          
024070     PERFORM IMS-GET-ARTC01                                               
024071     IF SEGMENT-FINNS                                                     
024080        MOVE ART-FLIART                 TO MOD-FLIART                     
024081        PERFORM IMS-GET-ARTC11                                            
024082        IF SEGMENT-FINNS                                                  
024083           MOVE CLAG-IDPSN              TO MOD-IDPSN-UT                   
024084                                           MOD-IDPSN-DOLT                 
024085           MOVE CLAG-IDRITN             TO MOD-IDRITN                     
024086           MOVE CLAG-KDFARLIG           TO MOD-KDFARLIG                   
024087           MOVE CLAG-KDERS              TO MOD-KDERS                      
024088           MOVE CLAG-KDARTHNT           TO WS-KDARTHNT                    
024089           MOVE WS-KDARTHNT-H           TO MOD-KDARTHNT-H                 
024090        ELSE                                                              
024091           MOVE MFS-RENSA-FAELT         TO MOD-IDPSN-UT                   
024092                                           MOD-IDRITN                     
024093                                           MOD-KDFARLIG                   
024094                                           MOD-KDERS                      
024095                                           MOD-KDARTHNT-H                 
024108        END-IF                                                            
024109     ELSE                                                                 
024110        MOVE MFS-RENSA-FAELT            TO MOD-FLIART                     
024111                                           MOD-KDERS                      
024112                                           MOD-IDPSN-UT                   
024113                                           MOD-IDRITN                     
024114                                           MOD-KDARTHNT-H                 
024115                                           MOD-KDFARLIG                   
024116     END-IF                                                               
024117                                                                          
024118     PERFORM IMS-GET-BENA01                                               
024119     IF SEGMENT-FINNS                                                     
024120        MOVE 'GB ' TO W-IDSKYLT                                           
024121        PERFORM IMS-GET-BENA11                                            
024122        IF SEGMENT-FINNS                                                  
024123           MOVE BENA11-TEXT-BEART       TO MOD-BEART-GB                   
024124        ELSE                                                              
024125           MOVE MFS-RENSA-FAELT         TO MOD-BEART-GB                   
024126        END-IF                                                            
024127        MOVE 'S  ' TO W-IDSKYLT                                           
024128        PERFORM IMS-GET-BENA11                                            
024129        IF SEGMENT-FINNS                                                  
024130           MOVE BENA11-TEXT-BEART       TO MOD-BEART-S                    
024131        ELSE                                                              
024132           MOVE MFS-RENSA-FAELT         TO MOD-BEART-S                    
024133        END-IF                                                            
024134     ELSE                                                                 
024135        MOVE MFS-RENSA-FAELT            TO MOD-BEART-S                    
024136                                           MOD-BEART-GB                   
024137     END-IF                                                               
024138                                                                          
024139     PERFORM IMS-GET-W6KVAH01                                             
024140     IF SEGMENT-FINNS                                                     
024141        PERFORM IMS-GET-W6KVAH11                                          
024142        IF SEGMENT-FINNS                                                  
024143           MOVE JA                      TO MOD-FL-KH                      
024144        ELSE                                                              
024145           MOVE MFS-RENSA-FAELT         TO MOD-FL-KH                      
024146        END-IF                                                            
024147     ELSE                                                                 
024148        MOVE MFS-RENSA-FAELT            TO MOD-FL-KH                      
024149     END-IF                                                               
024150                                                                          
024151     PERFORM IMS-GET-W6KVAG01                                             
024152     IF SEGMENT-FINNS                                                     
024153        MOVE JA                         TO MOD-FL-KR                      
024154     ELSE                                                                 
024155        MOVE NEJ                        TO MOD-FL-KR                      
024156     END-IF                                                               
024157     .                                                                    
024158     EJECT                                                                
024159 G-KOLLA-INPUT SECTION.                                                   
024160******************************************************************        
024161* KONTROLL    - NYUPPLÄGG/BORTTAG/UPPDATERING AV ARTIKEL ARTN01  *        
024162*             - UPPDATERING AV IDPSN ARTC11                      *        
024163******************************************************************        
024164                                                                          
024165     MOVE JA TO INDATA-SW                                                 
024166                                                                          
024167     IF MID-INPUT = ALL '+' AND MID-FLBORT = ALL '+'                      
024168        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSINF                         
024169        CALL WMEDKONV USING MED-WMEDAREA                                  
024170        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
024171        PERFORM MFS-ROER-EJ-FAELT-IN                                      
024172        PERFORM MFS-ROER-EJ-FAELT-UT                                      
024173        PERFORM MFS-ROER-EJ-FAELT-IN-UT                                   
024174        MOVE NEJ TO INDATA-SW                                             
024175     ELSE                                                                 
024176                                                                          
024177        PERFORM IMS-GET-ARTN01                                            
024178        IF SEGMENT-FINNS                                                  
024179           MOVE JA TO ARTIKEL-SW                                          
024180           PERFORM GB-KOLLA-INPUT-REPLACE                                 
024181        ELSE                                                              
024182           MOVE NEJ TO ARTIKEL-SW                                         
024183           PERFORM GA-KOLLA-INPUT-NYUPPLAGG                               
024184        END-IF                                                            
024185                                                                          
024186        IF MID-FLBORT = ALL '+'                                           
024187           MOVE MFS-RENSA-FAELT TO MOD-FLBORT                             
024188        ELSE                                                              
024189           IF MID-INPUT = ALL '+'                                         
024190              IF MID-FLBORT = JA                                          
024191                 IF ARTIKEL-SW = JA                                       
024192                    MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLBORT-ATTR          
024193                 ELSE                                                     
024194                    MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBORT-ATTR            
024195                    MOVE NEJ TO INDATA-SW                                 
024196                 END-IF                                                   
024197              ELSE                                                        
024198                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBORT-ATTR               
024199                 MOVE NEJ TO INDATA-SW                                    
024200              END-IF                                                      
024201           ELSE                                                           
024202              MOVE MFS-ALFA-FAELT-FEL TO MOD-FLBORT-ATTR                  
024203              MOVE NEJ TO INDATA-SW                                       
024204           END-IF                                                         
024205        END-IF                                                            
024206                                                                          
024207        IF INDATA-FEL                                                     
024208           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSINF                      
024209           CALL WMEDKONV USING MED-WMEDAREA                               
024210           MOVE MED-MFSINF TO MOD-TEMFSINF                                
024211           PERFORM MFS-ROER-EJ-FAELT-UT                                   
024212           PERFORM MFS-ROER-EJ-FAELT-IN                                   
024213           PERFORM MFS-ROER-EJ-FAELT-IN-UT                                
024214        END-IF                                                            
024215     END-IF                                                               
024216     .                                                                    
024217     EJECT                                                                
024218 GA-KOLLA-INPUT-NYUPPLAGG SECTION.                                        
024219                                                                          
024220     PERFORM S01-KOLLA-GEMENSAM-INPUT                                     
024221                                                                          
024222     IF MID-KVNTOFG = ALL '+'                                             
024223        MOVE MFS-RENSA-FAELT TO MOD-KVNTOFG-IN                            
024224        IF MID-KDSORT-KVNTOFG = ALL '+'                                   
024225           MOVE MFS-RENSA-FAELT TO MOD-KDSORT-KVNTOFG-IN                  
024226        ELSE                                                              
024227           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-KVNTOFG-ATTR             
024228           MOVE NEJ TO INDATA-SW                                          
024229        END-IF                                                            
024230     ELSE                                                                 
024231        MOVE MID-KVNTOFG  TO DEC-IDFRIDATA                                
024232        MOVE 2            TO DEC-KVHELTAL                                 
024233        MOVE 3            TO DEC-KVDECIMAL                                
024234                                                                          
024235        CALL WDECEDIT USING DEC-WDECAREA                                  
024236                                                                          
024237        IF DEC-KDSVAR-OK                                                  
024238           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVNTOFG-ATTR                  
024239           MOVE DEC-IDEDITDATA       TO WS-KVNTOFG                        
024240        ELSE                                                              
024241           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVNTOFG-ATTR                    
024242           MOVE NEJ TO INDATA-SW                                          
024243        END-IF                                                            
024244                                                                          
024245        IF MID-KDSORT-KVNTOFG = ALL '+'                                   
024246           MOVE MFS-RENSA-FAELT    TO MOD-KDSORT-KVNTOFG-IN               
024247           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-KVNTOFG-ATTR             
024248           MOVE NEJ TO INDATA-SW                                          
024249        ELSE                                                              
024250           IF MID-KDSORT-KVNTOFG = 'KG' OR 'M ' OR 'L '                   
024251              MOVE MFS-ALFA-FAELT-RAETT TO                                
024252                                MOD-KDSORT-KVNTOFG-ATTR                   
024253           ELSE                                                           
024254              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-KVNTOFG-ATTR          
024255              MOVE NEJ TO INDATA-SW                                       
024256           END-IF                                                         
024257        END-IF                                                            
024260     END-IF                                                               
024385                                                                          
024403     IF MID-VLFG = ALL '+'                                                
024404        MOVE MFS-RENSA-FAELT TO MOD-VLFG-IN                               
024405        IF MID-KDSORT-VLFG = ALL '+'                                      
024406           MOVE MFS-RENSA-FAELT TO MOD-KDSORT-VLFG-IN                     
024407        ELSE                                                              
024408           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-VLFG-ATTR                
024409           MOVE NEJ TO INDATA-SW                                          
024410        END-IF                                                            
024411     ELSE                                                                 
024412        MOVE MID-VLFG TO DEC-IDFRIDATA                                    
024413        MOVE 4        TO DEC-KVHELTAL                                     
024414        MOVE 3        TO DEC-KVDECIMAL                                    
024415                                                                          
024416        CALL WDECEDIT USING DEC-WDECAREA                                  
024417                                                                          
024418        IF DEC-KDSVAR-OK                                                  
024419           MOVE MFS-ALFA-FAELT-RAETT TO MOD-VLFG-ATTR                     
024420           MOVE DEC-IDEDITDATA       TO WS-VLFG                           
024421        ELSE                                                              
024422           MOVE MFS-ALFA-FAELT-FEL TO MOD-VLFG-ATTR                       
024423           MOVE NEJ TO INDATA-SW                                          
024424        END-IF                                                            
024425                                                                          
024426        IF MID-KDSORT-VLFG = ALL '+'                                      
024427           MOVE MFS-RENSA-FAELT TO MOD-KDSORT-VLFG-IN                     
024428           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-VLFG-ATTR                
024429           MOVE NEJ TO INDATA-SW                                          
024430        ELSE                                                              
024431           IF MID-KDSORT-VLFG = 'KG  ' OR 'L   ' OR 'KG G'                
024432              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-VLFG-ATTR           
024433           ELSE                                                           
024434              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-VLFG-ATTR             
024435              MOVE NEJ TO INDATA-SW                                       
024436           END-IF                                                         
024437        END-IF                                                            
024438     END-IF                                                               
024439                                                                          
024522     .                                                                    
024523     EJECT                                                                
024524 GB-KOLLA-INPUT-REPLACE SECTION.                                          
024525                                                                          
024526     PERFORM S01-KOLLA-GEMENSAM-INPUT                                     
024527                                                                          
024528     IF MID-KVNTOFG = ALL '+'                                             
024529        MOVE MFS-RENSA-FAELT TO MOD-KVNTOFG-IN                            
024530        IF MID-KDSORT-KVNTOFG = ALL '+'                                   
024531           MOVE MFS-RENSA-FAELT TO MOD-KDSORT-KVNTOFG-IN                  
024539        ELSE                                                              
024540           IF MID-KDSORT-KVNTOFG = 'KG' OR 'M ' OR 'L '                   
024541              MOVE MFS-ALFA-FAELT-RAETT TO                                
024542                                MOD-KDSORT-KVNTOFG-ATTR                   
024543           ELSE                                                           
024544              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-KVNTOFG-ATTR          
024545              MOVE NEJ TO INDATA-SW                                       
024546           END-IF                                                         
024547        END-IF                                                            
024548     ELSE                                                                 
024549        MOVE MID-KVNTOFG  TO DEC-IDFRIDATA                                
024550        MOVE 2            TO DEC-KVHELTAL                                 
024551        MOVE 3            TO DEC-KVDECIMAL                                
024552                                                                          
024553        CALL WDECEDIT USING DEC-WDECAREA                                  
024554                                                                          
024555        IF DEC-KDSVAR-OK                                                  
024556           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVNTOFG-ATTR                  
024557           MOVE DEC-IDEDITDATA       TO WS-KVNTOFG                        
024558        ELSE                                                              
024559           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVNTOFG-ATTR                    
024560           MOVE NEJ TO INDATA-SW                                          
024561        END-IF                                                            
024562                                                                          
024563        IF MID-KDSORT-KVNTOFG = ALL '+'                                   
024564           MOVE MFS-RENSA-FAELT TO MOD-KDSORT-KVNTOFG-IN                  
024565           IF ARTN01-ART-KDSORT-KVNTOFG = SPACE                           
024566             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-KVNTOFG-ATTR           
024567             MOVE NEJ TO INDATA-SW                                        
024568           END-IF                                                         
024569        ELSE                                                              
024570           IF MID-KDSORT-KVNTOFG = 'KG' OR 'M ' OR 'L '                   
024571              MOVE MFS-ALFA-FAELT-RAETT TO                                
024572                                MOD-KDSORT-KVNTOFG-ATTR                   
024573           ELSE                                                           
024574              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-KVNTOFG-ATTR          
024575              MOVE NEJ TO INDATA-SW                                       
024576           END-IF                                                         
024577        END-IF                                                            
024578     END-IF                                                               
024579                                                                          
024580     IF MID-VLFG = ALL '+'                                                
024581        MOVE MFS-RENSA-FAELT TO MOD-VLFG-IN                               
024582        IF MID-KDSORT-VLFG = ALL '+'                                      
024583           MOVE MFS-RENSA-FAELT TO MOD-KDSORT-VLFG-IN                     
024584        ELSE                                                              
024585           IF MID-KDSORT-VLFG = 'KG  ' OR 'L   ' OR 'KG G'                
024586              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-VLFG-ATTR           
024587           ELSE                                                           
024588              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-VLFG-ATTR             
024589              MOVE NEJ TO INDATA-SW                                       
024590           END-IF                                                         
024591        END-IF                                                            
024592     ELSE                                                                 
024593        MOVE MID-VLFG TO DEC-IDFRIDATA                                    
024594        MOVE 4        TO DEC-KVHELTAL                                     
024595        MOVE 3        TO DEC-KVDECIMAL                                    
024596                                                                          
024597        CALL WDECEDIT USING DEC-WDECAREA                                  
024598                                                                          
024599        IF DEC-KDSVAR-OK                                                  
024600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-VLFG-ATTR                     
024601           MOVE DEC-IDEDITDATA       TO WS-VLFG                           
024602        ELSE                                                              
024603           MOVE MFS-ALFA-FAELT-FEL TO MOD-VLFG-ATTR                       
024604           MOVE NEJ TO INDATA-SW                                          
024605        END-IF                                                            
024606                                                                          
024607        IF MID-KDSORT-VLFG = ALL '+'                                      
024608           MOVE MFS-RENSA-FAELT TO MOD-KDSORT-VLFG-IN                     
024609           IF ARTN01-ART-KDSORT-VLFG = SPACE                              
024610             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-VLFG-ATTR              
024611             MOVE NEJ TO INDATA-SW                                        
024612           END-IF                                                         
024613        ELSE                                                              
024614           IF MID-KDSORT-VLFG = 'KG' OR 'L '  OR 'KG G'                   
024615              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSORT-VLFG-ATTR           
024616           ELSE                                                           
024617              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDSORT-VLFG-ATTR             
024618              MOVE NEJ TO INDATA-SW                                       
024619           END-IF                                                         
024620        END-IF                                                            
024630     END-IF                                                               
024674     .                                                                    
024675     EJECT                                                                
024676 H-UPPDATERA SECTION.                                                     
024677******************************************************************        
024678* FUNKTIONER  - NYUPPLÄGG/BORTTAG/UPPDATERING AV ARTIKEL ARTN01  *        
024679*             - UPPDATERING AV IDPSN ARTC11                      *        
024680******************************************************************        
024681                                                                          
024683     PERFORM IMS-GET-ARTN01                                               
024684     IF SEGMENT-FINNS                                                     
024685        IF MID-FLBORT = JA                                                
024686           PERFORM IMS-DLET-ARTN                                          
024688        ELSE                                                              
024689           PERFORM HB-UPPDATERING-ARTIKEL                                 
024690        END-IF                                                            
024691     ELSE                                                                 
024692        PERFORM HA-NYUPPLAGG-ARTIKEL                                      
024693     END-IF                                                               
024694                                                                          
024695     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
024696     CALL WMEDKONV USING MED-WMEDAREA                                     
024697     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
024698     PERFORM MFS-FORM-ATTR-IN                                             
024699     PERFORM MFS-RENSA-FAELT-IN                                           
024702     MOVE NEJ TO WS-TENOTE1-IFYLLT                                        
024703                 WS-TENOTE2-IFYLLT                                        
024707     .                                                                    
024708     EJECT                                                                
024709 HA-NYUPPLAGG-ARTIKEL SECTION.                                            
024710                                                                          
024711     MOVE W-IDARTNR                TO ARTN01-ART-IDARTNR                  
024712     MOVE DAGENS-DATUM             TO ARTN01-ART-TIREGDAT                 
024713                                                                          
024714     IF MID-BEEMBMAT = ALL '+'                                            
024715        MOVE SPACE                 TO ARTN01-ART-BEEMBMAT                 
024716     ELSE                                                                 
024717        MOVE MID-BEEMBMAT          TO ARTN01-ART-BEEMBMAT                 
024718        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEEMBMAT-UT-ATTR                
024719     END-IF                                                               
024720                                                                          
024721     IF MID-IDANMNR = ALL '+'                                             
024722        MOVE ZERO                  TO ARTN01-ART-IDANMNR                  
024723     ELSE                                                                 
024724        MOVE WS-IDANMNR            TO ARTN01-ART-IDANMNR                  
024725        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDANMNR-UT-ATTR                 
024726     END-IF                                                               
024727                                                                          
024728     IF MID-REKSIFFR-ANMNR = ALL '+'                                      
024729        MOVE ZERO                  TO ARTN01-ART-REKSIFFR-ANMNR           
024730     ELSE                                                                 
024731        MOVE WS-REKSIFFR           TO ARTN01-ART-REKSIFFR-ANMNR           
024732        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-REKSIFFR-UT-ATTR                
024733     END-IF                                                               
024734                                                                          
024735     IF MID-VKART-FG = ALL '+'                                            
024736        MOVE ZERO                  TO ARTN01-ART-VKART-FG                 
024737     ELSE                                                                 
024738        MOVE WS-VKART-FG           TO ARTN01-ART-VKART-FG                 
024739        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VKART-FG-UT-ATTR                
024740     END-IF                                                               
024741                                                                          
024742     IF MID-FLFROST = ALL '+'                                             
024743        MOVE NEJ                   TO ARTN01-ART-FLFROST                  
024744     ELSE                                                                 
024745        MOVE MID-FLFROST           TO ARTN01-ART-FLFROST                  
024746     END-IF                                                               
024747     MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLFROST-UT-ATTR                 
024748                                                                          
024749     IF MID-FLTACTIL = ALL '+'                                            
024750        MOVE NEJ                   TO ARTN01-ART-FLTACTIL                 
024751     ELSE                                                                 
024752        MOVE MID-FLTACTIL          TO ARTN01-ART-FLTACTIL                 
024753     END-IF                                                               
024754     MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLTACTIL-UT-ATTR                
024755                                                                          
024756     IF MID-FLVARINF = ALL '+'                                            
024757        MOVE SPACE                 TO ARTN01-ART-FLVARINF                 
024758     ELSE                                                                 
024759        MOVE MID-FLVARINF          TO ARTN01-ART-FLVARINF                 
024760     END-IF                                                               
024761     MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLVARINF-UT-ATTR                
024762                                                                          
024763     IF MID-FLVARINF-SDS = ALL '+'                                        
024764        MOVE SPACE                 TO ARTN01-ART-FLVARINF-SDS             
024765     ELSE                                                                 
024766        MOVE MID-FLVARINF-SDS      TO ARTN01-ART-FLVARINF-SDS             
024767     END-IF                                                               
024768     MOVE MFS-ADD-LYS-UPP-FAELT    TO MOD-FLVARINF-SDS-UT-ATTR            
024769                                                                          
024770     IF MID-IDAO-FG = ALL '+'                                             
024771        MOVE SPACE                 TO ARTN01-ART-IDAO-FG                  
024772     ELSE                                                                 
024773        MOVE MID-IDAO-FG           TO ARTN01-ART-IDAO-FG                  
024774        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDAO-FG-UT-ATTR                 
024775     END-IF                                                               
024776                                                                          
024777     IF MID-IDVARINF = ALL '+'                                            
024778        MOVE SPACE                 TO ARTN01-ART-IDVARINF                 
024779     ELSE                                                                 
024780        MOVE MID-IDVARINF          TO ARTN01-ART-IDVARINF                 
024781        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDVARINF-UT-ATTR                
024782     END-IF                                                               
024783                                                                          
024784     IF MID-IDVARINF-SDS = ALL '+'                                        
024785        MOVE SPACE                 TO ARTN01-ART-IDVARINF-SDS             
024786     ELSE                                                                 
024787        MOVE MID-IDVARINF-SDS      TO ARTN01-ART-IDVARINF-SDS             
024788        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDVARINF-SDS-UT-ATTR            
024789     END-IF                                                               
024790                                                                          
024791     IF MID-KDFARG = ALL '+'                                              
024792        MOVE ZERO                  TO ARTN01-ART-KDFARG                   
024793     ELSE                                                                 
024794        MOVE MID-KDFARG            TO ARTN01-ART-KDFARG                   
024795        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFARG-UT-ATTR                  
024796     END-IF                                                               
024797                                                                          
024798     IF MID-KDFGPRIO = ALL '+'                                            
024799        MOVE ZERO                  TO ARTN01-ART-KDFGPRIO                 
024800     ELSE                                                                 
024801        MOVE WS-KDFGPRIO           TO ARTN01-ART-KDFGPRIO                 
024802        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFGPRIO-UT-ATTR                
024803     END-IF                                                               
024804                                                                          
024805     IF MID-KDLACK = ALL '+'                                              
024806        MOVE SPACE                 TO ARTN01-ART-KDLACK                   
024807     ELSE                                                                 
024808        MOVE MID-KDLACK            TO ARTN01-ART-KDLACK                   
024809        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDLACK-UT-ATTR                  
024810     END-IF                                                               
024811                                                                          
024812     IF MID-IDARTNR-RECEPT = ALL '+'                                      
024813        MOVE ZERO                  TO ARTN01-ART-IDARTNR-RECEPT           
024814     ELSE                                                                 
024815        MOVE WS-IDARTNR-RECEPT     TO ARTN01-ART-IDARTNR-RECEPT           
024816        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-RECEPT-UT-ATTR          
024817     END-IF                                                               
024818                                                                          
024819     IF MID-VKFORSFG = ALL '+'                                            
024820        MOVE ZERO                  TO ARTN01-ART-VKFORSFG                 
024821     ELSE                                                                 
024822        MOVE WS-VKFORSFG           TO ARTN01-ART-VKFORSFG                 
024823        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VKFORSFG-UT-ATTR                
024824     END-IF                                                               
024825                                                                          
024826     IF MID-VLFG = ALL '+'                                                
024827        MOVE ZERO                  TO ARTN01-ART-VLFG                     
024828        MOVE SPACE                 TO ARTN01-ART-KDSORT-VLFG              
024829     ELSE                                                                 
024830        MOVE WS-VLFG               TO ARTN01-ART-VLFG                     
024831        MOVE MID-KDSORT-VLFG       TO ARTN01-ART-KDSORT-VLFG              
024832        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VLFG-UT-ATTR                    
024833                                      MOD-KDSORT-VLFG-UT-ATTR             
024834     END-IF                                                               
024835                                                                          
024836     IF MID-KVFLAMP = ALL '+'                                             
024837        MOVE ZERO                  TO ARTN01-ART-KVFLAMP                  
024838     ELSE                                                                 
024839        IF MID-FLNEG = 'M'                                                
024840           COMPUTE WS-KVFLAMP-NEG = WS-KVFLAMP * -1.0                     
024841           MOVE WS-KVFLAMP-NEG     TO ARTN01-ART-KVFLAMP                  
024842        ELSE                                                              
024843           MOVE WS-KVFLAMP         TO ARTN01-ART-KVFLAMP                  
024844        END-IF                                                            
024845        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVFLAMP-UT-ATTR                 
024846     END-IF                                                               
024847                                                                          
024848     IF MID-KVNTOFG = ALL '+'                                             
024849        MOVE ZERO                  TO ARTN01-ART-KVNTOFG                  
024850        MOVE SPACE                 TO ARTN01-ART-KDSORT-KVNTOFG           
024851     ELSE                                                                 
024852        MOVE WS-KVNTOFG            TO ARTN01-ART-KVNTOFG                  
024853        MOVE MID-KDSORT-KVNTOFG    TO ARTN01-ART-KDSORT-KVNTOFG           
024854        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVNTOFG-UT-ATTR                 
024855                                      MOD-KDSORT-KVNTOFG-UT-ATTR          
024856     END-IF                                                               
024857                                                                          
024858     IF MID-KVVOC = ALL '+'                                               
024859        MOVE ZERO                  TO ARTN01-ART-KVVOC                    
024860     ELSE                                                                 
024861        MOVE WS-KVVOC              TO ARTN01-ART-KVVOC                    
024862        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVVOC-UT-ATTR                   
024863     END-IF                                                               
024864                                                                          
024865     IF MID-SUEQFG = ALL '+'                                              
024866        MOVE ZERO                  TO ARTN01-ART-SUEQFG                   
024867     ELSE                                                                 
024868        MOVE WS-SUEQFG             TO ARTN01-ART-SUEQFG                   
024869        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-SUEQFG-UT-ATTR                  
024870     END-IF                                                               
024871                                                                          
024872     IF MID-TENOTE(1) = ALL '+'                                           
024873        MOVE SPACE                 TO ARTN01-ART-TENOTE(1)                
024874     ELSE                                                                 
024875        MOVE MID-TENOTE(1)         TO ARTN01-ART-TENOTE(1)                
024876        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TENOTE-ATTR(1)                  
024877     END-IF                                                               
024878                                                                          
024879     IF MID-TENOTE(2) = ALL '+'                                           
024880        MOVE SPACE                 TO ARTN01-ART-TENOTE(2)                
024881     ELSE                                                                 
024882        MOVE MID-TENOTE(2)         TO ARTN01-ART-TENOTE(2)                
024883        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TENOTE-ATTR(2)                  
024884     END-IF                                                               
024885                                                                          
024886     PERFORM IMS-ISRT-ARTN01                                              
024887     PERFORM S02-UPPDATERA-ARTC11                                         
024888     .                                                                    
024889     EJECT                                                                
024890 HB-UPPDATERING-ARTIKEL SECTION.                                          
024891                                                                          
024892     IF MID-BEEMBMAT = ALL '+'                                            
024893        CONTINUE                                                          
024894     ELSE                                                                 
024895        MOVE MID-BEEMBMAT          TO ARTN01-ART-BEEMBMAT                 
024896        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BEEMBMAT-UT-ATTR                
024897     END-IF                                                               
024898                                                                          
024899     IF MID-IDANMNR = ALL '+'                                             
024900        CONTINUE                                                          
024901     ELSE                                                                 
024902        MOVE WS-IDANMNR            TO ARTN01-ART-IDANMNR                  
024903        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDANMNR-UT-ATTR                 
024904     END-IF                                                               
024905                                                                          
024906     IF MID-REKSIFFR-ANMNR = ALL '+'                                      
024907        CONTINUE                                                          
024908     ELSE                                                                 
024909        MOVE WS-REKSIFFR           TO ARTN01-ART-REKSIFFR-ANMNR           
024910        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-REKSIFFR-UT-ATTR                
024911     END-IF                                                               
024912                                                                          
024913     IF MID-VKART-FG = ALL '+'                                            
024914        CONTINUE                                                          
024915     ELSE                                                                 
024916        MOVE WS-VKART-FG           TO ARTN01-ART-VKART-FG                 
024917        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VKART-FG-UT-ATTR                
024918     END-IF                                                               
024919                                                                          
024920     IF MID-FLFROST = ALL '+'                                             
024921        CONTINUE                                                          
024922     ELSE                                                                 
024923        MOVE MID-FLFROST           TO ARTN01-ART-FLFROST                  
024924        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLFROST-UT-ATTR                 
024925     END-IF                                                               
024926                                                                          
024927     IF MID-FLTACTIL = ALL '+'                                            
024928        CONTINUE                                                          
024929     ELSE                                                                 
024930        MOVE MID-FLTACTIL          TO ARTN01-ART-FLTACTIL                 
024931        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLTACTIL-UT-ATTR                
024932     END-IF                                                               
024933                                                                          
024934     IF MID-FLVARINF = ALL '+'                                            
024935        CONTINUE                                                          
024936     ELSE                                                                 
024937        MOVE MID-FLVARINF          TO ARTN01-ART-FLVARINF                 
024938        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLVARINF-UT-ATTR                
024939     END-IF                                                               
024940                                                                          
024941     IF MID-FLVARINF-SDS = ALL '+'                                        
024942        CONTINUE                                                          
024943     ELSE                                                                 
024944        MOVE MID-FLVARINF-SDS      TO ARTN01-ART-FLVARINF-SDS             
024945        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLVARINF-SDS-UT-ATTR            
024946     END-IF                                                               
024947                                                                          
024948     IF MID-IDAO-FG = ALL '+'                                             
024949        CONTINUE                                                          
024950     ELSE                                                                 
024951        MOVE MID-IDAO-FG           TO ARTN01-ART-IDAO-FG                  
024952        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDAO-FG-UT-ATTR                 
024953     END-IF                                                               
024954                                                                          
024955     IF MID-IDVARINF = ALL '+'                                            
024956        CONTINUE                                                          
024957     ELSE                                                                 
024958        MOVE MID-IDVARINF          TO ARTN01-ART-IDVARINF                 
024959        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDVARINF-UT-ATTR                
024960     END-IF                                                               
024961                                                                          
024962     IF MID-IDVARINF-SDS = ALL '+'                                        
024963        CONTINUE                                                          
024964     ELSE                                                                 
024965        MOVE MID-IDVARINF-SDS      TO ARTN01-ART-IDVARINF-SDS             
024966        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDVARINF-SDS-UT-ATTR            
024967     END-IF                                                               
024968                                                                          
024969     IF MID-KDFARG = ALL '+'                                              
024970        CONTINUE                                                          
024971     ELSE                                                                 
024972        MOVE MID-KDFARG            TO ARTN01-ART-KDFARG                   
024973        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFARG-UT-ATTR                  
024974     END-IF                                                               
024975                                                                          
024976     IF MID-KDSORT-KVNTOFG = ALL '+'                                      
024977        CONTINUE                                                          
024978     ELSE                                                                 
024979        MOVE MID-KDSORT-KVNTOFG    TO ARTN01-ART-KDSORT-KVNTOFG           
024980        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDSORT-KVNTOFG-UT-ATTR          
024981     END-IF                                                               
024982                                                                          
024983     IF MID-KDSORT-VLFG = ALL '+'                                         
024984        CONTINUE                                                          
024985     ELSE                                                                 
024986        MOVE MID-KDSORT-VLFG       TO ARTN01-ART-KDSORT-VLFG              
024987        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDSORT-VLFG-UT-ATTR             
024988     END-IF                                                               
024989                                                                          
024990     IF MID-KDFGPRIO = ALL '+'                                            
024991        CONTINUE                                                          
024992     ELSE                                                                 
024993        MOVE WS-KDFGPRIO           TO ARTN01-ART-KDFGPRIO                 
024994        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFGPRIO-UT-ATTR                
024995     END-IF                                                               
024996                                                                          
024997     IF MID-KDLACK = ALL '+'                                              
024998        CONTINUE                                                          
024999     ELSE                                                                 
025000        MOVE MID-KDLACK            TO ARTN01-ART-KDLACK                   
025001        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDLACK-UT-ATTR                  
025002     END-IF                                                               
025003                                                                          
025004     IF MID-IDARTNR-RECEPT = ALL '+'                                      
025005        CONTINUE                                                          
025006     ELSE                                                                 
025007        MOVE WS-IDARTNR-RECEPT     TO ARTN01-ART-IDARTNR-RECEPT           
025008        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-RECEPT-UT-ATTR          
025009     END-IF                                                               
025010                                                                          
025011     IF MID-VKFORSFG = ALL '+'                                            
025012        CONTINUE                                                          
025013     ELSE                                                                 
025014        MOVE WS-VKFORSFG           TO ARTN01-ART-VKFORSFG                 
025015        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VKFORSFG-UT-ATTR                
025016     END-IF                                                               
025017                                                                          
025018     IF MID-VLFG = ALL '+'                                                
025019        CONTINUE                                                          
025020     ELSE                                                                 
025021        MOVE WS-VLFG               TO ARTN01-ART-VLFG                     
025022        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VLFG-UT-ATTR                    
025023     END-IF                                                               
025024                                                                          
025025     IF MID-KVFLAMP = ALL '+'                                             
025026        CONTINUE                                                          
025027     ELSE                                                                 
025028        IF MID-FLNEG = 'M'                                                
025029           COMPUTE WS-KVFLAMP-NEG = WS-KVFLAMP * -1.0                     
025030           MOVE WS-KVFLAMP-NEG     TO ARTN01-ART-KVFLAMP                  
025031        ELSE                                                              
025032           MOVE WS-KVFLAMP         TO ARTN01-ART-KVFLAMP                  
025033        END-IF                                                            
025034        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVFLAMP-UT-ATTR                 
025035     END-IF                                                               
025036                                                                          
025037     IF MID-KVNTOFG = ALL '+'                                             
025038        CONTINUE                                                          
025039     ELSE                                                                 
025040        MOVE WS-KVNTOFG            TO ARTN01-ART-KVNTOFG                  
025041        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVNTOFG-UT-ATTR                 
025042     END-IF                                                               
025043                                                                          
025044     IF MID-KVVOC = ALL '+'                                               
025045        CONTINUE                                                          
025046     ELSE                                                                 
025047        MOVE WS-KVVOC              TO ARTN01-ART-KVVOC                    
025048        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVVOC-UT-ATTR                   
025049     END-IF                                                               
025050                                                                          
025051     IF MID-SUEQFG = ALL '+'                                              
025052        CONTINUE                                                          
025053     ELSE                                                                 
025054        MOVE WS-SUEQFG             TO ARTN01-ART-SUEQFG                   
025055        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-SUEQFG-UT-ATTR                  
025056     END-IF                                                               
025057                                                                          
025058     IF MID-TENOTE(1) = ALL '+'                                           
025059        CONTINUE                                                          
025060     ELSE                                                                 
025061        MOVE MID-TENOTE(1)         TO ARTN01-ART-TENOTE(1)                
025062        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TENOTE-ATTR(1)                  
025063     END-IF                                                               
025064                                                                          
025065     IF MID-TENOTE(2) = ALL '+'                                           
025066        CONTINUE                                                          
025067     ELSE                                                                 
025068        MOVE MID-TENOTE(2)         TO ARTN01-ART-TENOTE(2)                
025069        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TENOTE-ATTR(2)                  
025070     END-IF                                                               
025071                                                                          
025072     PERFORM IMS-REPL-ARTN                                                
025073     PERFORM S02-UPPDATERA-ARTC11                                         
025074     .                                                                    
025075     EJECT                                                                
025076 S01-KOLLA-GEMENSAM-INPUT SECTION.                                        
025077                                                                          
025078     MOVE NEJ TO WS-TENOTE1-IFYLLT                                        
025079                 WS-TENOTE2-IFYLLT                                        
025080                                                                          
025081     IF MID-KDFARG = ALL '+'                                              
025082        MOVE MFS-RENSA-FAELT TO MOD-KDFARG-IN                             
025083     ELSE                                                                 
025084        IF MID-KDFARG NUMERIC                                             
025085           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFARG-ATTR                    
025086        ELSE                                                              
025087           MOVE MFS-NUM-FAELT-FEL TO MOD-KDFARG-ATTR                      
025088           MOVE NEJ TO INDATA-SW                                          
025089        END-IF                                                            
025090     END-IF                                                               
025091                                                                          
025092     IF MID-KDLACK = ALL '+'                                              
025093        MOVE MFS-RENSA-FAELT TO MOD-KDLACK-IN                             
025094     ELSE                                                                 
025095        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDLACK-ATTR                      
025096     END-IF                                                               
025097                                                                          
025098     IF MID-IDARTNR-RECEPT = ALL '+'                                      
025099        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-RECEPT-IN                     
025100     ELSE                                                                 
025101        IF MID-IDARTNR-RECEPT NUMERIC                                     
025102           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-RECEPT-ATTR            
025103           MOVE MID-IDARTNR-RECEPT TO WS-IDARTNR-RECEPT                   
025104        ELSE                                                              
025105           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-RECEPT-ATTR              
025106           MOVE NEJ TO INDATA-SW                                          
025107        END-IF                                                            
025108     END-IF                                                               
025109                                                                          
025110     IF MID-VKFORSFG = ALL '+'                                            
025111        MOVE MFS-RENSA-FAELT TO MOD-VKFORSFG-IN                           
025112     ELSE                                                                 
025113        MOVE MID-VKFORSFG  TO DEC-IDFRIDATA                               
025114        MOVE 4             TO DEC-KVHELTAL                                
025115        MOVE 3             TO DEC-KVDECIMAL                               
025116                                                                          
025117        CALL WDECEDIT USING DEC-WDECAREA                                  
025118                                                                          
025119        IF DEC-KDSVAR-OK                                                  
025120           MOVE MFS-ALFA-FAELT-RAETT TO MOD-VKFORSFG-ATTR                 
025121           MOVE DEC-IDEDITDATA       TO WS-VKFORSFG                       
025122        ELSE                                                              
025123           MOVE MFS-ALFA-FAELT-FEL TO MOD-VKFORSFG-ATTR                   
025124           MOVE NEJ TO INDATA-SW                                          
025125        END-IF                                                            
025126     END-IF                                                               
025127                                                                          
025128     IF MID-FLVARINF = ALL '+'                                            
025129        MOVE MFS-RENSA-FAELT TO MOD-FLVARINF-IN                           
025130     ELSE                                                                 
025131        IF MID-FLVARINF = JA OR NEJ OR SPACE                              
025132           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLVARINF-ATTR                 
025133        ELSE                                                              
025134           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLVARINF-ATTR                   
025135           MOVE NEJ TO INDATA-SW                                          
025136        END-IF                                                            
025137     END-IF                                                               
025138                                                                          
025139     IF MID-FLVARINF-SDS = ALL '+'                                        
025140        MOVE MFS-RENSA-FAELT TO MOD-FLVARINF-SDS-IN                       
025141     ELSE                                                                 
025142        IF MID-FLVARINF-SDS = JA OR NEJ OR SPACE                          
025143           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLVARINF-SDS-ATTR             
025144        ELSE                                                              
025145           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLVARINF-SDS-ATTR               
025146           MOVE NEJ TO INDATA-SW                                          
025147        END-IF                                                            
025148     END-IF                                                               
025149                                                                          
025150     IF MID-IDVARINF = ALL '+'                                            
025151        MOVE MFS-RENSA-FAELT TO MOD-IDVARINF-IN                           
025152     ELSE                                                                 
025153        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDVARINF-ATTR                    
025154     END-IF                                                               
025155                                                                          
025156     IF MID-IDVARINF-SDS = ALL '+'                                        
025157        MOVE MFS-RENSA-FAELT TO MOD-IDVARINF-SDS-IN                       
025158     ELSE                                                                 
025159        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDVARINF-SDS-ATTR                
025160     END-IF                                                               
025161                                                                          
025162     IF MID-KVVOC = ALL '+'                                               
025163        MOVE MFS-RENSA-FAELT TO MOD-KVVOC-IN                              
025164     ELSE                                                                 
025165        MOVE MID-KVVOC  TO DEC-IDFRIDATA                                  
025166        MOVE 2          TO DEC-KVHELTAL                                   
025167        MOVE 3          TO DEC-KVDECIMAL                                  
025168                                                                          
025169        CALL WDECEDIT USING DEC-WDECAREA                                  
025170                                                                          
025171        IF DEC-KDSVAR-OK                                                  
025172           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVVOC-ATTR                    
025173           MOVE DEC-IDEDITDATA       TO WS-KVVOC                          
025174        ELSE                                                              
025175           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVVOC-ATTR                      
025176           MOVE NEJ TO INDATA-SW                                          
025177        END-IF                                                            
025178     END-IF                                                               
025179                                                                          
025180     IF MID-SUEQFG = ALL '+'                                              
025181        MOVE MFS-RENSA-FAELT TO MOD-SUEQFG-IN                             
025182     ELSE                                                                 
025183        MOVE MID-SUEQFG TO DEC-IDFRIDATA                                  
025184        MOVE 3          TO DEC-KVHELTAL                                   
025185        MOVE 4          TO DEC-KVDECIMAL                                  
025186                                                                          
025187        CALL WDECEDIT USING DEC-WDECAREA                                  
025188                                                                          
025189        IF DEC-KDSVAR-OK                                                  
025190           MOVE MFS-ALFA-FAELT-RAETT TO MOD-SUEQFG-ATTR                   
025191           MOVE DEC-IDEDITDATA       TO WS-SUEQFG                         
025192        ELSE                                                              
025193           MOVE MFS-ALFA-FAELT-FEL TO MOD-SUEQFG-ATTR                     
025194           MOVE NEJ TO INDATA-SW                                          
025195        END-IF                                                            
025196     END-IF                                                               
025197                                                                          
025198     IF MID-FLNEG = ALL '+'                                               
025199        MOVE MFS-RENSA-FAELT TO MOD-FLNEG-IN                              
025200     ELSE                                                                 
025201        IF MID-FLNEG = 'P' OR 'M'                                         
025202           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLNEG-ATTR                    
025203        ELSE                                                              
025204           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNEG-ATTR                      
025205           MOVE NEJ TO INDATA-SW                                          
025206        END-IF                                                            
025207     END-IF                                                               
025208                                                                          
025209     IF MID-KVFLAMP = ALL '+'                                             
025210        MOVE MFS-RENSA-FAELT TO MOD-KVFLAMP-IN                            
025211        IF MID-FLNEG = ALL '+'                                            
025212           MOVE MFS-RENSA-FAELT TO MOD-FLNEG-IN                           
025213        ELSE                                                              
025214           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLNEG-ATTR                      
025215           MOVE NEJ TO INDATA-SW                                          
025216        END-IF                                                            
025217     ELSE                                                                 
025218        IF MID-KVFLAMP NUMERIC                                            
025219           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVFLAMP-ATTR                   
025220           MOVE MID-KVFLAMP TO WS-KVFLAMP                                 
025221        ELSE                                                              
025222           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVFLAMP-ATTR                    
025223           MOVE NEJ TO INDATA-SW                                          
025224        END-IF                                                            
025225     END-IF                                                               
025226                                                                          
025227     IF MID-FLFROST = ALL '+'                                             
025228        MOVE MFS-RENSA-FAELT TO MOD-FLFROST-IN                            
025229     ELSE                                                                 
025230        IF MID-FLFROST = JA OR NEJ                                        
025231           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLFROST-ATTR                  
025232        ELSE                                                              
025233           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFROST-ATTR                    
025234           MOVE NEJ TO INDATA-SW                                          
025235        END-IF                                                            
025236     END-IF                                                               
025237                                                                          
025238     IF MID-FLTACTIL = ALL '+'                                            
025239        MOVE MFS-RENSA-FAELT TO MOD-FLTACTIL-IN                           
025240     ELSE                                                                 
025241        IF MID-FLTACTIL = JA OR NEJ                                       
025242           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTACTIL-ATTR                 
025243        ELSE                                                              
025244           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLTACTIL-ATTR                   
025245           MOVE NEJ TO INDATA-SW                                          
025246        END-IF                                                            
025247     END-IF                                                               
025248                                                                          
025249     IF MID-IDPSN = ALL '+'                                               
025250        MOVE MFS-RENSA-FAELT TO MOD-IDPSN-IN                              
025251     ELSE                                                                 
025252        IF MID-IDPSN NUMERIC                                              
025253           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDPSN-ATTR                     
025254        ELSE                                                              
025255           MOVE MFS-NUM-FAELT-FEL TO MOD-IDPSN-ATTR                       
025256           MOVE NEJ TO INDATA-SW                                          
025257        END-IF                                                            
025258     END-IF                                                               
025259                                                                          
025260     IF MID-IDAO-FG = ALL '+'                                             
025261        MOVE MFS-RENSA-FAELT TO MOD-IDAO-FG-IN                            
025262     ELSE                                                                 
025263        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDAO-FG-ATTR                     
025264     END-IF                                                               
025265                                                                          
025266     IF MID-KDFGPRIO = ALL '+'                                            
025267        MOVE MFS-RENSA-FAELT TO MOD-KDFGPRIO-IN                           
025268     ELSE                                                                 
025269        IF MID-KDFGPRIO NUMERIC                                           
025270           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFGPRIO-ATTR                  
025271           MOVE MID-KDFGPRIO TO WS-KDFGPRIO                               
025272        ELSE                                                              
025273           MOVE MFS-NUM-FAELT-FEL TO MOD-KDFGPRIO-ATTR                    
025274           MOVE NEJ TO INDATA-SW                                          
025275        END-IF                                                            
025276     END-IF                                                               
025277                                                                          
025278     IF MID-IDANMNR = ALL '+'                                             
025279        MOVE MFS-RENSA-FAELT TO MOD-IDANMNR-IN                            
025280     ELSE                                                                 
025281        IF MID-IDANMNR NUMERIC                                            
025282           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANMNR-ATTR                   
025283           MOVE MID-IDANMNR  TO WS-IDANMNR                                
025284        ELSE                                                              
025285           MOVE MFS-NUM-FAELT-FEL TO MOD-IDANMNR-ATTR                     
025286           MOVE NEJ TO INDATA-SW                                          
025287        END-IF                                                            
025288     END-IF                                                               
025289                                                                          
025290     IF MID-REKSIFFR-ANMNR = ALL '+'                                      
025291        MOVE MFS-RENSA-FAELT TO MOD-REKSIFFR-IN                           
025292     ELSE                                                                 
025293        IF MID-REKSIFFR-ANMNR NUMERIC                                     
025294           MOVE MFS-NUM-FAELT-RAETT TO MOD-REKSIFFR-ATTR                  
025295           MOVE MID-REKSIFFR-ANMNR TO WS-REKSIFFR                         
025296        ELSE                                                              
025297           MOVE MFS-NUM-FAELT-FEL TO MOD-REKSIFFR-ATTR                    
025298           MOVE NEJ TO INDATA-SW                                          
025299        END-IF                                                            
025300     END-IF                                                               
025301                                                                          
025302     IF MID-VKART-FG = ALL '+'                                            
025303        MOVE MFS-RENSA-FAELT TO MOD-VKART-FG-IN                           
025304     ELSE                                                                 
025305        IF MID-VKART-FG NUMERIC                                           
025306           MOVE MFS-NUM-FAELT-RAETT TO MOD-VKART-FG-ATTR                  
025307           MOVE MID-VKART-FG TO WS-VKART-FG                               
025308        ELSE                                                              
025309           MOVE MFS-NUM-FAELT-FEL TO MOD-VKART-FG-ATTR                    
025310           MOVE NEJ TO INDATA-SW                                          
025311        END-IF                                                            
025312     END-IF                                                               
025313                                                                          
025314     IF MID-BEEMBMAT = ALL '+'                                            
025315        MOVE MFS-RENSA-FAELT TO MOD-BEEMBMAT-IN                           
025316     ELSE                                                                 
025317        MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEEMBMAT-ATTR                    
025318     END-IF                                                               
025319                                                                          
025320     IF MID-TENOTE(1)= ALL '+'                                            
025321        MOVE MFS-RENSA-FAELT TO MOD-TENOTE(1)                             
025322     ELSE                                                                 
025323        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TENOTE-ATTR(1)                   
025324        MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE(1)                           
025325        MOVE JA TO WS-TENOTE1-IFYLLT                                      
025326     END-IF                                                               
025327                                                                          
025328     IF MID-TENOTE(2)= ALL '+'                                            
025329        MOVE MFS-RENSA-FAELT TO MOD-TENOTE(2)                             
025330     ELSE                                                                 
025331        MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE(2)                           
025332        MOVE MFS-ALFA-FAELT-RAETT TO MOD-TENOTE-ATTR(2)                   
025333        MOVE JA TO WS-TENOTE2-IFYLLT                                      
025334     END-IF                                                               
025335     .                                                                    
025336     EJECT                                                                
025337 S02-UPPDATERA-ARTC11 SECTION.                                            
025338                                                                          
025339     IF MID-IDPSN = ALL '+'                                               
025340        CONTINUE                                                          
025341     ELSE                                                                 
025342        PERFORM IMS-GET-ARTC01                                            
025343        IF SEGMENT-FINNS                                                  
025344           PERFORM IMS-GET-ARTC11                                         
025345           IF SEGMENT-FINNS                                               
025346              MOVE MID-IDPSN TO CLAG-IDPSN                                
025347              PERFORM IMS-REPL-ARTC                                       
025348              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDPSN-UT-ATTR             
025349           END-IF                                                         
025350        END-IF                                                            
025351     END-IF                                                               
025352     .                                                                    
025353     EJECT                                                                
025354 MFS-RENSA-FAELT-UT SECTION.                                              
025355                                                                          
025356     MOVE MFS-RENSA-FAELT TO MOD-BEART-S                                  
025357                             MOD-BEART-GB                                 
025358                             MOD-KDARTHNT-H                               
025359                             MOD-KDFARLIG                                 
025360                             MOD-FLIART                                   
025361                             MOD-KDERS                                    
025362                             MOD-FL-KH                                    
025363                             MOD-FL-KR                                    
025364                             MOD-IDRITN                                   
025365                             MOD-IDPSN-UT                                 
025366                             MOD-BEEMBMAT-UT                              
025367                             MOD-FLFROST-UT                               
025368                             MOD-FLTACTIL-UT                              
025369                             MOD-FLVARINF-UT                              
025370                             MOD-FLVARINF-SDS-UT                          
025371                             MOD-IDVARINF-UT                              
025372                             MOD-IDVARINF-SDS-UT                          
025373                             MOD-KDFARG-UT                                
025374                             MOD-KDFGPRIO-UT                              
025375                             MOD-KDLACK-UT                                
025376                             MOD-KDSORT-KVNTOFG-UT                        
025377                             MOD-KDSORT-VLFG-UT                           
025378                             MOD-KVFLAMP-UT                               
025379                             MOD-KVNTOFG-UT                               
025380                             MOD-KVVOC-UT                                 
025381                             MOD-IDARTNR-RECEPT-UT                        
025382                             MOD-SUEQFG-UT                                
025383                             MOD-VKFORSFG-UT                              
025384                             MOD-VLFG-UT                                  
025385                             MOD-IDAO-FG-UT                               
025386                             MOD-VKART-FG-UT                              
025387                             MOD-IDANMNR-UT                               
025388                             MOD-REKSIFFR-UT                              
025389     .                                                                    
025390     EJECT                                                                
025391 MFS-RENSA-FAELT-IN SECTION.                                              
025400                                                                          
025600     MOVE MFS-RENSA-FAELT TO MOD-IDPSN-IN                                 
025620                             MOD-BEEMBMAT-IN                              
025630                             MOD-FLFROST-IN                               
025640                             MOD-FLTACTIL-IN                              
025650                             MOD-FLVARINF-IN                              
025660                             MOD-FLVARINF-SDS-IN                          
025670                             MOD-IDVARINF-IN                              
025680                             MOD-IDVARINF-SDS-IN                          
025690                             MOD-KDFARG-IN                                
025691                             MOD-KDFGPRIO-IN                              
025692                             MOD-KDLACK-IN                                
025693                             MOD-KDSORT-KVNTOFG-IN                        
025694                             MOD-KDSORT-VLFG-IN                           
025695                             MOD-FLNEG-IN                                 
025696                             MOD-KVFLAMP-IN                               
025697                             MOD-KVNTOFG-IN                               
025698                             MOD-KVVOC-IN                                 
025699                             MOD-IDARTNR-RECEPT-IN                        
025700                             MOD-SUEQFG-IN                                
025701                             MOD-VKFORSFG-IN                              
025702                             MOD-VLFG-IN                                  
025703                             MOD-IDAO-FG-IN                               
025704                             MOD-FLBORT                                   
025705                             MOD-TENOTE(1)                                
025706                             MOD-TENOTE(2)                                
025707                             MOD-VKART-FG-IN                              
025708                             MOD-IDANMNR-IN                               
025709                             MOD-REKSIFFR-IN                              
025800     .                                                                    
025900     EJECT                                                                
026000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026100                                                                          
026310     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-S                                
026320                               MOD-BEART-GB                               
026330                               MOD-KDARTHNT-H                             
026340                               MOD-KDFARLIG                               
026350                               MOD-FLIART                                 
026360                               MOD-KDERS                                  
026370                               MOD-FL-KH                                  
026380                               MOD-FL-KR                                  
026390                               MOD-IDRITN                                 
026391                               MOD-IDPSN-UT                               
026392                               MOD-BEEMBMAT-UT                            
026393                               MOD-FLFROST-UT                             
026394                               MOD-FLTACTIL-UT                            
026395                               MOD-FLVARINF-UT                            
026396                               MOD-FLVARINF-SDS-UT                        
026397                               MOD-IDVARINF-UT                            
026398                               MOD-IDVARINF-SDS-UT                        
026399                               MOD-KDFARG-UT                              
026400                               MOD-KDFGPRIO-UT                            
026401                               MOD-KDLACK-UT                              
026402                               MOD-KDSORT-KVNTOFG-UT                      
026403                               MOD-KDSORT-VLFG-UT                         
026404                               MOD-KVFLAMP-UT                             
026405                               MOD-KVNTOFG-UT                             
026406                               MOD-KVVOC-UT                               
026407                               MOD-IDARTNR-RECEPT-UT                      
026408                               MOD-SUEQFG-UT                              
026409                               MOD-VKFORSFG-UT                            
026410                               MOD-VLFG-UT                                
026413                               MOD-IDAO-FG-UT                             
026414                               MOD-VKART-FG-UT                            
026415                               MOD-IDANMNR-UT                             
026416                               MOD-REKSIFFR-UT                            
026820     .                                                                    
026900     EJECT                                                                
027000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027100                                                                          
027410     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPSN-IN                               
027420                               MOD-BEEMBMAT-IN                            
027430                               MOD-FLFROST-IN                             
027440                               MOD-FLTACTIL-IN                            
027450                               MOD-FLVARINF-IN                            
027460                               MOD-FLVARINF-SDS-IN                        
027470                               MOD-IDVARINF-IN                            
027480                               MOD-IDVARINF-SDS-IN                        
027490                               MOD-KDFARG-IN                              
027491                               MOD-KDFGPRIO-IN                            
027492                               MOD-KDLACK-IN                              
027493                               MOD-KDSORT-KVNTOFG-IN                      
027494                               MOD-KDSORT-VLFG-IN                         
027495                               MOD-FLNEG-IN                               
027496                               MOD-KVFLAMP-IN                             
027497                               MOD-KVNTOFG-IN                             
027498                               MOD-KVVOC-IN                               
027499                               MOD-IDARTNR-RECEPT-IN                      
027500                               MOD-SUEQFG-IN                              
027501                               MOD-VKFORSFG-IN                            
027502                               MOD-VLFG-IN                                
027503                               MOD-IDAO-FG-IN                             
027504                               MOD-VKART-FG-IN                            
027505                               MOD-IDANMNR-IN                             
027506                               MOD-REKSIFFR-IN                            
027530     .                                                                    
027600     EJECT                                                                
027610 MFS-ROER-EJ-FAELT-IN-UT SECTION.                                         
027611                                                                          
027612     IF MID-TENOTE(1) = ALL '+'                                           
027613        CONTINUE                                                          
027614     ELSE                                                                 
027615        MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE(1)                           
027616        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-ATTR(1)                  
027617        MOVE JA TO WS-TENOTE1-IFYLLT                                      
027618     END-IF                                                               
027619                                                                          
027620     IF MID-TENOTE(2) = ALL '+'                                           
027621        CONTINUE                                                          
027622     ELSE                                                                 
027623        MOVE MFS-ROER-EJ-FAELT TO MOD-TENOTE(2)                           
027624        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-ATTR(2)                  
027625        MOVE JA TO WS-TENOTE2-IFYLLT                                      
027626     END-IF                                                               
027627                                                                          
027628     IF MID-FLBORT = ALL '+'                                              
027629        CONTINUE                                                          
027630     ELSE                                                                 
027631        MOVE MFS-ROER-EJ-FAELT TO MOD-FLBORT                              
027632     END-IF                                                               
027633     .                                                                    
027640     EJECT                                                                
027700 MFS-FORM-ATTR-IN SECTION.                                                
027800                                                                          
028000     MOVE MFS-FORMATETS-ATTR TO MOD-IDPSN-ATTR                            
028100                                MOD-BEEMBMAT-ATTR                         
028110                                MOD-FLFROST-ATTR                          
028120                                MOD-FLTACTIL-ATTR                         
028130                                MOD-FLVARINF-ATTR                         
028140                                MOD-FLVARINF-SDS-ATTR                     
028150                                MOD-IDAO-FG-ATTR                          
028160                                MOD-IDVARINF-ATTR                         
028170                                MOD-IDVARINF-SDS-ATTR                     
028180                                MOD-KDFARG-ATTR                           
028190                                MOD-KDLACK-ATTR                           
028191                                MOD-KDFGPRIO-ATTR                         
028192                                MOD-KDSORT-KVNTOFG-ATTR                   
028193                                MOD-KDSORT-VLFG-ATTR                      
028194                                MOD-KVFLAMP-ATTR                          
028195                                MOD-KVNTOFG-ATTR                          
028196                                MOD-KVVOC-ATTR                            
028197                                MOD-IDARTNR-RECEPT-ATTR                   
028198                                MOD-SUEQFG-ATTR                           
028199                                MOD-VKFORSFG-ATTR                         
028200                                MOD-VLFG-ATTR                             
028202                                MOD-FLBORT-ATTR                           
028203                                MOD-VKART-FG-ATTR                         
028204                                MOD-IDANMNR-ATTR                          
028205                                MOD-REKSIFFR-ATTR                         
028210     .                                                                    
028300     EJECT                                                                
029100* --- IMS SEKTIONER ---                                                   
029200     SKIP3                                                                
029300 IMS-GET-MSG SECTION.                                                     
029500     MOVE '  QC' TO GODK-STATUSKODER                                      
029600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
029700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029800     PERFORM IMS-STATUSKONTROLL                                           
029900     .                                                                    
030000     SKIP3                                                                
030100 IMS-INSERT-MSG SECTION.                                                  
030200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
030300        MOVE '0' TO MFS-KDHUVOMR                                          
030400     END-IF                                                               
030600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
030700     MOVE SPACE TO GODK-STATUSKODER                                       
030800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
030900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031000     PERFORM IMS-STATUSKONTROLL                                           
031100     .                                                                    
031201     EJECT                                                                
031202 IMS-GET-ARTN01 SECTION.                                                  
031203     STRING 'WLARTN01(IDARTNR  =' W-IDARTNR-X ')'                         
031204          DELIMITED BY SIZE INTO SSA1                                     
031205     MOVE '  GE' TO GODK-STATUSKODER                                      
031206     CALL CBLTDLI USING GHU ARTN-PCB DLI-IO-AREA2 SSA1                    
031207     MOVE ARTN-STATUS-CODE TO STATUS-WS                                   
031208     PERFORM IMS-STATUSKONTROLL                                           
031209     .                                                                    
031210     SKIP3                                                                
031211 IMS-ISRT-ARTN01 SECTION.                                                 
031213     MOVE 'WLARTN01 ' TO SSA1                                             
031214     MOVE '  ' TO GODK-STATUSKODER                                        
031215     CALL CBLTDLI USING ISRT ARTN-PCB DLI-IO-AREA2 SSA1                   
031216     MOVE ARTN-STATUS-CODE TO STATUS-WS                                   
031217     PERFORM IMS-STATUSKONTROLL                                           
031218     .                                                                    
031219     SKIP3                                                                
031220 IMS-REPL-ARTN SECTION.                                                   
031222     MOVE '  ' TO GODK-STATUSKODER                                        
031223     CALL CBLTDLI USING REPL ARTN-PCB DLI-IO-AREA2                        
031224     MOVE ARTN-STATUS-CODE TO STATUS-WS                                   
031225     PERFORM IMS-STATUSKONTROLL                                           
031226     .                                                                    
031227     SKIP3                                                                
031228 IMS-DLET-ARTN SECTION.                                                   
031230     MOVE '  ' TO GODK-STATUSKODER                                        
031231     CALL CBLTDLI USING DLET ARTN-PCB DLI-IO-AREA2                        
031232     MOVE ARTN-STATUS-CODE TO STATUS-WS                                   
031233     PERFORM IMS-STATUSKONTROLL                                           
031234     .                                                                    
031235     EJECT                                                                
031236 IMS-GET-ARTC01 SECTION.                                                  
031237     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031238          DELIMITED BY SIZE INTO SSA1                                     
031239     MOVE '  GE' TO GODK-STATUSKODER                                      
031240     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
031241     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031242     PERFORM IMS-STATUSKONTROLL                                           
031243     .                                                                    
031244     SKIP3                                                                
031245 IMS-GET-ARTC11 SECTION.                                                  
031247     MOVE 'WLARTC11 ' TO SSA1                                             
031249     MOVE '  GE' TO GODK-STATUSKODER                                      
031250     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
031251     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031252     PERFORM IMS-STATUSKONTROLL                                           
031253     .                                                                    
031254     SKIP3                                                                
031273 IMS-REPL-ARTC SECTION.                                                   
031274     MOVE '  ' TO GODK-STATUSKODER                                        
031275     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA                         
031276     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031277     PERFORM IMS-STATUSKONTROLL                                           
031278     .                                                                    
031301     EJECT                                                                
031302 IMS-GET-BENA01 SECTION.                                                  
031303     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
031304          DELIMITED BY SIZE INTO SSA1                                     
031305     MOVE '  GE' TO GODK-STATUSKODER                                      
031306     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
031307     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
031308     PERFORM IMS-STATUSKONTROLL                                           
031309     .                                                                    
031310     SKIP3                                                                
031311 IMS-GET-BENA11 SECTION.                                                  
031312     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
031313          DELIMITED BY SIZE INTO SSA1                                     
031314     MOVE '  GE' TO GODK-STATUSKODER                                      
031315     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
031316     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
031317     PERFORM IMS-STATUSKONTROLL                                           
031318     .                                                                    
031319     EJECT                                                                
031320 IMS-GET-W6KVAH01 SECTION.                                                
031321     STRING 'W6KVAH01(IDARTNR  =' W-IDARTNR-X ')'                         
031322          DELIMITED BY SIZE INTO SSA1                                     
031323     MOVE '  GE' TO GODK-STATUSKODER                                      
031324     CALL CBLTDLI USING GU KVAH-PCB DLI-IO-AREA SSA1                      
031325     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
031326     PERFORM IMS-STATUSKONTROLL                                           
031327     .                                                                    
031328     SKIP3                                                                
031329 IMS-GET-W6KVAH11 SECTION.                                                
031330     MOVE 'W6KVAH11 ' TO SSA1                                             
031331     MOVE '  GE' TO GODK-STATUSKODER                                      
031332     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA SSA1                     
031333     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
031334     PERFORM IMS-STATUSKONTROLL                                           
031335     .                                                                    
031336     SKIP3                                                                
031337 IMS-GET-W6KVAG01 SECTION.                                                
031338     STRING 'W6KVAG01(W6H7B1KY>=' W-W6H7B1KY-MIN-X                        
031339                    '&W6H7B1KY<=' W-W6H7B1KY-MAX-X ')'                    
031340          DELIMITED BY SIZE INTO SSA1                                     
031341     MOVE '  GE' TO GODK-STATUSKODER                                      
031342     CALL CBLTDLI USING GU KVAG-PCB DLI-IO-AREA SSA1                      
031343     MOVE KVAG-STATUS-CODE TO STATUS-WS                                   
031344     PERFORM IMS-STATUSKONTROLL                                           
031345     .                                                                    
031350     EJECT                                                                
031400 IMS-STATUSKONTROLL SECTION.                                              
031500                                                                          
031600     SET STATUS-IX TO 1                                                   
031700     SEARCH GODK-STATUS                                                   
031800       AT END                                                             
031900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032000         DELIMITED BY SIZE INTO FELTEXT                                   
032100         CALL FELLOG                                                      
032200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032300         CONTINUE                                                         
032400     END-SEARCH                                                           
032500     .                                                                    
