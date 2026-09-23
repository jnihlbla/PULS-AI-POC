001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     WL012600.                                                
001400 AUTHOR.         SUBBARAO PARUCHURI V.                                    
001500 DATE-WRITTEN.   04/08/11.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800*    NAME:       'CARPARTS.LDC.CONFIRMZEROES'                             
001900*                                                                         
002000*    FUNCTION:                                                            
002200*        BEKRÄFTA NOLLOR, ALLA KLASSER.                                   
002300*                                                                         
002400* WL012600 PROGRAM IS A REPLICA OF W4032500 PROGRAM                       
002410* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSACTION: WL0126T                                             
002800*        REQUEST:     WL0126I1                                            
002900*                                                                         
003000*    OUTDATA.                                                             
003100*        RESPONSE:    WL0126O1                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400 FILE SECTION.                                                            
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800 77  IDPGM                       PIC X(08)   VALUE 'WL012600'.            
004900                                                                          
005000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005200 77  KDRC-DISPLAY                PIC Z(5).                                
005300                                                                          
005400 77    JA                        PIC X       VALUE 'J'.                   
005500 77    NEJ                       PIC X       VALUE 'N'.                   
005600 77    SPRAK-INDX                PIC S9(9)   VALUE +0   COMP SYNC.        
005700 77    MAX-LINE                  PIC S9(9)   VALUE +13  COMP SYNC.        
005800 77    MAX-MOD-LAENGD            PIC S9(4)  VALUE +191  COMP SYNC.        
005900                                                                          
006000 77    WS-KDMFSFOR               PIC 9       VALUE ZERO.                  
006100 77    WS-KDFEL                  PIC 9(2)    VALUE ZERO COMP-3.           
006200 77    IDPW-WS                   PIC X(8).                                
006300 77    WS-IDUSER                 PIC X(7)    VALUE 'NOLLJAG'.             
006400 77    WS-EGEN-BILD              PIC X(4)    VALUE '4325'.                
006500 77    WS-PASSWORD-OK            PIC X(1)    VALUE 'N'.                   
006510 77    WS-RAD-FUNNEN             PIC X(1)    VALUE 'N'.                   
006520 77    WS-OGAE12-FINNS           PIC X(1)    VALUE 'N'.                   
006530 77    WS-IDKUNDRF               PIC X(10).                               
006540 77    WS-SPAR-KDORDSTA          PIC S9(1)              COMP-3.           
006550 77    WS-SPAR-KVORDRAD-LEVPL    PIC S9(5)   VALUE ZERO COMP-3.           
006560 77    SPAR-IDPRODNR             PIC S9(7)   VALUE ZERO COMP-3.           
006570 77    SPAR-IDPLKLST             PIC S9(3)   VALUE ZERO COMP-3.           
006580 77    SPAR-IDUSER               PIC X(8)    VALUE ZERO.                  
006590 77    TREFF                     PIC X       VALUE 'N'.                   
006591 77    WS-KORD-IDDC              PIC X(2).                                
006592                                                                          
006593 77    WS-IDELMT-ERROR           PIC X(16).                               
006594 77    WS-IDMSG-ERROR            PIC X(03).                               
006595 77    WS-IDMSG-INFO             PIC X(03).                               
006596*                                                                         
006601 01    DAGENS-DATUM              PIC S9(6).                               
006602                                                                          
006603 01    WS-IDANSTNR                           PIC X(5).                    
006604 01    IDANSTNR-WS REDEFINES WS-IDANSTNR     PIC 9(5).                    
006605 01    WS-IDDISTR                            PIC X(4).                    
006606 01    IDDISTR-WS REDEFINES WS-IDDISTR       PIC 9(4).                    
006607 01    WS-IDKUNDNR                           PIC X(6).                    
006608 01    IDKUNDNR-WS REDEFINES WS-IDKUNDNR     PIC 9(6).                    
006609 01    WS-IDORDNR                            PIC X(5).                    
006610 01    IDORDNR-WS REDEFINES WS-IDORDNR       PIC 9(5).                    
006611 01    WS-IDPRODNR                           PIC X(7).                    
006612 01    IDPRODNR-WS REDEFINES WS-IDPRODNR     PIC 9(7).                    
006613 01    WS-IDRADNR                            PIC X(4).                    
006614 01    IDRADNR-WS REDEFINES WS-IDRADNR       PIC 9(4).                    
006615 01    WS-IDARTNR                            PIC X(8).                    
006616 01    IDARTNR-WS  REDEFINES WS-IDARTNR      PIC 9(8).                    
006617 01    IDPLKLST-WS                           PIC 9(3).                    
006618**********ARBETSFÄLT FÖR ANNULLATION*******************                   
006619 01    WS-FLJANEJ                            PIC X.                       
006620     EJECT                                                                
006700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007200     03  WSECURIT                PIC X(8)    VALUE 'WSECURIT'.            
007210     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     SKIP3                                                                
007400*    --- PARAMETERS TO ABEND                                              
007500                                                                          
007600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008000     SKIP3                                                                
008100 01  MESSAGE-CODES.                                                       
008300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
008800     EJECT                                                                
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009100     SKIP3                                                                
009200*01  -COPY WZ01SUB                                                        
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009500     SKIP3                                                                
009600 01  REQU-AREA.                                                           
009700*    03  -COPY WZ01REQU                                                   
009800*    03  -COPY WL0126I1                                                   
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010100     SKIP3                                                                
010200 01  RESP-AREA.                                                           
010300*    03  -COPY WZ01RESP                                                   
010400*    03  -COPY WL0126O1                                                   
010500     EJECT                                                                
010600 77    WS-SPARA-IDRADNR-FROM     PIC 9(5)    VALUE ZERO COMP-3.           
010700 77    WS-SPARA-IDRADNR-TOM      PIC 9(5)    VALUE ZERO COMP-3.           
010710     SKIP2                                                                
010720 77    SUM-KVUTRS                PIC S9(7)   VALUE +0   COMP-3.           
010730     SKIP2                                                                
010740 01    NYCKEL-ID                 PIC 9(2)    COMP-3.                      
010750   88  NYCKEL-IDPRODNR           VALUE 1.                                 
010760   88  NYCKEL-ORDERID            VALUE 2.                                 
010770     SKIP2                                                                
010780 01    FRAN-BILD                 PIC 9(4).                                
010790   88  FRAN-BILD-OK              VALUE 4325.                              
010791     SKIP2                                                                
010792 01    WS-SLINGA-KLAR            PIC X(1).                                
010793   88  SLINGA-KLAR               VALUE 'J'.                               
010794     EJECT                                                                
010795 01 FILLER                       PIC X(16) VALUE 'WDE420-SPAR '.          
010796 01  -COPY WDE411 -PRE SPAR-.                                             
010797     EJECT                                                                
010798 01    NYCKLAR-TILL-DLI.                                                  
010799   03    W-WDE601-IDPRODNR-X.                                             
010800     05    W-WDE601-IDPRODNR     PIC S9(7)            COMP-3.             
010810                                                                          
010820   03    W-IDPRODNR-X.                                                    
010830     05    W-IDPRODNR            PIC S9(7)            COMP-3.             
010840                                                                          
010850   03    W-IDANSTNR-X.                                                    
010860     05    W-IDANSTNR            PIC S9(5)            COMP-3.             
010870                                                                          
010880   03    W-IDRADNR-X.                                                     
010890     05    W-IDRADNR             PIC S9(5)            COMP-3.             
010891                                                                          
010892   03    W-IDRADNR-ISRT-X.                                                
010893     05    W-IDRADNR-ISRT        PIC S9(5)            COMP-3.             
010894                                                                          
010895   03    W-IDARTNR-X.                                                     
010896     05    W-IDARTNR             PIC S9(9)            COMP-3.             
010897                                                                          
010898   03    W-KDSEGKEY-X.                                                    
010899     05    W-KDSEGKEY            PIC X                VALUE '1'.          
010900                                                                          
010910   03    W-WDE4A1-KUNDORDER-X.                                            
010920     05    W-4A1-IDDISTR         PIC S9(5)            COMP-3.             
010930     05    W-4A1-IDKUNDNR        PIC S9(7)            COMP-3.             
010940     05    W-4A1-IDKUNDRF.                                                
010950       07    W-4A1-IDORDNR       PIC X(5).                                
010960       07    FILLER              PIC X(5)   VALUE SPACE.                  
010970                                                                          
010980   03    W-WDE401-KUNDORDER-X.                                            
010990     05    W-401-IDDISTR         PIC S9(5)            COMP-3.             
010991     05    W-401-IDKUNDNR        PIC S9(7)            COMP-3.             
010992     05    W-401-IDKUNDRF.                                                
010993       07    W-401-IDORDNR       PIC X(5).                                
010994       07    FILLER              PIC X(5)   VALUE SPACE.                  
010995     05    W-401-IDPRODNR        PIC S9(7)            COMP-3.             
010996     05    W-401-IDPLKLST        PIC S9(3)            COMP-3.             
010997                                                                          
010998   03    W-WDE4B-KEYSEQ-MIN-X.                                            
010999     05    W-420-IDPRODNR-MIN    PIC S9(7)            COMP-3.             
011000     05    W-420-IDPURAD-MIN     PIC S9(5)            COMP-3.             
011010                                                                          
011020   03    W-WDE4B-KEYSEQ-MAX-X.                                            
011030     05    W-420-IDPRODNR-MAX    PIC S9(7)            COMP-3.             
011040     05    W-420-IDPURAD-MAX     PIC S9(5)            COMP-3.             
011050                                                                          
011060   03    W-WDE420-IDPURAD-X.                                              
011070     05    W-420-IDPURAD         PIC S9(5)            COMP-3.             
011080                                                                          
011090   03    W-IDDC-X.                                                        
011091     05    W-IDDC-ARTS           PIC X(2).                                
011092     EJECT                                                                
011093 01    MEDDELANDE.                                                        
011094   03    FEL-X1                  PIC X(3)    VALUE '099'.                 
011095   03    FEL-1                   PIC X(3)    VALUE '025'.                 
011096   03    FEL-2                   PIC X(3)    VALUE '161'.                 
011097   03    FEL-3                   PIC X(3)    VALUE '211'.                 
011098   03    FEL-4                   PIC X(3)    VALUE '025'.                 
011099   03    FEL-5                   PIC X(3)    VALUE '213'.                 
011100   03    FEL-6                   PIC X(3)    VALUE '214'.                 
011101   03    FEL-7                   PIC X(3)    VALUE '023'.                 
011102   03    FEL-8                   PIC X(3)    VALUE '025'.                 
011103   03    FEL-9                   PIC X(3)    VALUE '111'.                 
011104   03    FEL-10                  PIC X(3)    VALUE '146'.                 
011105   03    FEL-11                  PIC X(3)    VALUE '217'.                 
011106   03    FEL-12                  PIC X(3)    VALUE '00A'.                 
011107   03    FEL-13                  PIC X(3)    VALUE '218'.                 
011108   03    MED-2                   PIC X(3)    VALUE '219'.                 
011109   03    FEL-18                  PIC X(3)    VALUE '220'.                 
011110   03    FEL-19                  PIC X(3)    VALUE '190'.                 
011111   03    MED-1                   PIC X(3)    VALUE '001'.                 
011112   03    FEL-21                  PIC X(3)    VALUE '152'.                 
011113   03    FEL-26                  PIC X(3)    VALUE '026'.                 
011114   03    FEL-27                  PIC X(3)    VALUE '158'.                 
011115   03    FEL-28                  PIC X(3)    VALUE '222'.                 
011116   03    FEL-IDORDNR             PIC X(7)    VALUE 'IDORDNR'.             
011117   03    FEL-IDARTNR             PIC X(7)    VALUE 'IDARTNR'.             
011118   03    FEL-IDRADNR             PIC X(7)    VALUE 'IDRADNR'.             
011119   03    FEL-FLJANEJ             PIC X(7)    VALUE 'FLJANEJ'.             
011274     EJECT                                                                
011275*01   -COPY  WSECAREA                                                     
011276      EJECT                                                               
011277* - - - - - - - - - -  BYTES-OBJEKTTEST                                   
011278 01  TEST-IDARTNR              PIC 9(9)    COMP-3.                        
011279*01  FILLER -COPY WWBYT09      -RED TEST-IDARTNR                          
011280     EJECT                                                                
011281******************************************************************        
011282*                                                                         
011283*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011284*                                                                         
011285 01    IMS-WS.                                                            
011286   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
011287                                                                          
011288*                        **** STATUS-KOD FRÅN IMS                         
011289   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
011290     88    KUNDORDER-SEK-FINNS              VALUE '  '.                   
011291     88    KUNDORDER-SEK-SAKNAS             VALUE 'GE' 'GB'.              
011292   03    STATUS-WS               PIC XX.                                  
011293     88    SEGMENT-FINNS                    VALUE '  '.                   
011294     88    SEGMENT-SAKNAS                   VALUE 'GE'.                   
011295     88    SEGMENT-FINNS-REDAN              VALUE 'II'.                   
011296     88    SLUT-PA-BASEN                    VALUE 'GB'.                   
011297     SKIP3                                                                
011298*                        **** LEVEL-KOD FRÅN IMS                          
011299   03    LEVEL-WS                PIC X(2).                                
011300     88    ROT-SEG-SAKNAS                   VALUE '00'.                   
011301     SKIP3                                                                
011302   03    GODK-STATUSKODER.                                                
011303     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
011304     SKIP3                                                                
011305 01    SSA1                      PIC X(64).                               
011306 01    SSA2                      PIC X(64).                               
011307 01    SSA3                      PIC X(64).                               
011308     EJECT                                                                
011309*                            IMS FUNKTIONSKODER                           
011310*01    -COPY W0003                                                        
011311     EJECT                                                                
011312*                            DLI INPUT-OUTPUT AREA                        
011313 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011314 01    DLI-IO-AREA.                                                       
011315   03    IO-AREA                 PIC X(500)  VALUE SPACE.                 
011316     SKIP3                                                                
011317*  03    WDE401   -COPY WDE401              -RED IO-AREA.                 
011318     EJECT                                                                
011319*  03    WDE411   -COPY WDE411              -RED IO-AREA.                 
011320     EJECT                                                                
011321*                            DLI INPUT-OUTPUT AREA                        
011322 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
011323 01    DLI-IO-AREA2.                                                      
011324   03    IO-AREA2                PIC X(900)  VALUE SPACE.                 
011325     SKIP3                                                                
011326*  03    WLARTC11 -COPY WDK611 -RED IO-AREA2.                             
011327     EJECT                                                                
011328*  03    WLARTS11 -COPY WDK711 -RED IO-AREA2.                             
011329     EJECT                                                                
011330 01    DLI-IO-WDE601.                                                     
011331*  03    WDE601   -COPY WDE601.                                           
011332     EJECT                                                                
011333*                                                                         
011334 01  FILLER                     PIC X(16) VALUE 'ALT-IO-AREA'.            
011335 01    ALT-IO-AREA.                                                       
011336     03    ALT-LL               PIC S9(4) COMP SYNC VALUE +63.            
011337     03    ALT-Z1               PIC X     VALUE LOW-VALUE.                
011338     03    ALT-Z2               PIC X     VALUE LOW-VALUE.                
011339     03    ALT-TRANSKOD         PIC X(8)  VALUE 'W5T108X '.               
011340     03    ALT-IDTRANS          PIC X(4)  VALUE '4325'.                   
011341     03    ALT-KDMFSFOR         PIC X     VALUE SPACE.                    
011342     03    ALT-IDARTNR-IN       PIC X(9).                                 
011343     03    ALT-IDARTNR-UT       PIC X(9)  VALUE ZERO.                     
011344     03    ALT-IDPW-IN          PIC X(8).                                 
011345     03    ALT-IDPW-UT          PIC X(8)  VALUE SPACE.                    
011346     03    ALT-IDDC-IN          PIC X(2)  VALUE SPACE.                    
011347     03    ALT-IDDC-UT          PIC X(2)  VALUE SPACE.                    
011348     03    ALT-IDPRODNR         PIC 9(7)  VALUE ZERO.                     
011349     03    ALT-KDORDKL          PIC 9     VALUE ZERO.                     
011350                                                                          
011351     EJECT                                                                
011360 LINKAGE SECTION.                                                         
011400 01  MSG-PCB                     PIC X.                                   
011600     EJECT                                                                
011700*01    -COPY W0009     -PRE ALT-                                          
011701     EJECT                                                                
011702*01    -COPY W0008     -PRE WDE4-                                         
011703     05  FILLER                  PIC X.                                   
011704                                                                          
011705*01    -COPY W0008     -PRE WDE4A-                                        
011706     05  FILLER                  PIC X.                                   
011707     EJECT                                                                
011708*01    -COPY W0008     -PRE WDE6-                                         
011709     05  FILLER                  PIC X.                                   
011710     EJECT                                                                
011711*01    -COPY W0008     -PRE ARTC-                                         
011712     05  FILLER                  PIC X.                                   
011713                                                                          
011714*01    -COPY W0008     -PRE ARTS-                                         
011715     05  FILLER                  PIC X.                                   
011716     EJECT                                                                
011717*01    -COPY W0008     -PRE WDE4B-                                        
011718     05  FILLER                  PIC X.                                   
011719     EJECT                                                                
011800                                                                          
011900 PROCEDURE DIVISION USING MSG-PCB   ALT-PCB  WDE4-PCB WDE4A-PCB           
011910                          WDE6-PCB ARTC-PCB ARTS-PCB                      
011920                          WDE4B-PCB.                                      
011930 MAIN SECTION.                                                            
011940     ENTRY 'DLITCBL' USING MSG-PCB   ALT-PCB  WDE4-PCB WDE4A-PCB          
011950                           WDE6-PCB ARTC-PCB ARTS-PCB                     
011960                           WDE4B-PCB.                                     
011970                                                                          
011980     ACCEPT DAGENS-DATUM FROM DATE                                        
012000     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
012100     IF SUB-KDRC = 0                                                      
012110        IF REQU-KDPGMACT = 'E'                                            
012200           PERFORM A-INIT-SPARA-INPUT                                     
012400           PERFORM B-KOLLA-INPUT                                          
012500           IF WS-KDFEL = ZERO                                             
012600              PERFORM C-KONTROLL-AV-ORDERDEL                              
012700              IF WS-KDFEL = ZERO                                          
012800***FRÅGA OM ANNULLERING***                                                
012900                 IF WS-FLJANEJ = 'N'                                      
012910                    PERFORM F-UPPDAT-UTREDN-SALDO                         
012920                 END-IF                                                   
012930                 IF WS-KDFEL = ZERO                                       
012940                    PERFORM H-UPPDAT-NOLLJ-FLAGGA                         
012950                 END-IF                                                   
012960              END-IF                                                      
012970           END-IF                                                         
012980           IF WS-KDFEL > ZERO                                             
012990              PERFORM J-HAMTA-MEDDELANDE                                  
012991              IF (WS-KDFEL < 25) AND  (WS-KDFEL NOT = 21)                 
012993                 PERFORM K-VISA-BILD-IGEN                                 
012994              END-IF                                                      
012995           END-IF                                                         
012996           IF (WS-KDFEL = ZERO ) OR  (WS-KDFEL = 32)                      
012997              MOVE REQU-IDARTNR-IN   TO  RESP-IDARTNR-UT                  
012998              MOVE REQU-IDRADNR-IN   TO  RESP-IDRADNR-UT                  
012999              MOVE REQU-IDPW-IN      TO  RESP-IDPW-UT                     
013000              MOVE REQU-FLJANEJ-IN   TO  RESP-FLJANEJ-UT                  
013001                                                                          
013002              MOVE ZERO              TO  RESP-IDRADNR-IN                  
013003                                         RESP-IDARTNR-IN                  
013004              MOVE SPACE             TO  RESP-IDPW-IN                     
013005                                         RESP-FLJANEJ-IN                  
013009           END-IF                                                         
013013        ELSE                                                              
013014           MOVE FEL-X1       TO RESP-IDMSG-ERROR                          
013015        END-IF                                                            
013016                                                                          
013017        MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                          
013018        MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                         
013019        MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                        
013020                                                                          
013021        IF WS-IDMSG-ERROR NOT = SPACE                                     
013022           MOVE ALL '+' TO RESP-AREA                                      
013023           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
013024           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
013025           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
013026           MOVE 001              TO RESP-IDMSGVER                         
013027        END-IF                                                            
013028                                                                          
013030        PERFORM S02-RETURN-RESPONSE                                       
013100     END-IF                                                               
013200                                                                          
013600     MOVE ZERO TO RETURN-CODE                                             
013700     GOBACK                                                               
013800     .                                                                    
013900     EJECT                                                                
014000 A-INIT-SPARA-INPUT SECTION.                                              
014100                                                                          
015200     MOVE ZERO TO WS-KDFEL                                                
015510     MOVE ALL '+' TO RESP-AREA                                            
015520     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
015530                     RESP-IDMSG-INFO                                      
015540                     RESP-IDELMT-ERROR                                    
015550     MOVE 001   TO RESP-IDMSGVER                                          
015600     PERFORM AB-INIT-NYCKLAR                                              
017500     .                                                                    
017600     EJECT                                                                
017700 AB-INIT-NYCKLAR SECTION.                                                 
017800                                                                          
017900     IF REQU-IDPRODNR-KEY NOT = ALL '+'                                   
018010       MOVE REQU-IDPRODNR-KEY TO WS-IDPRODNR                              
018100     ELSE                                                                 
018110       MOVE ZERO              TO WS-IDPRODNR                              
018400     END-IF                                                               
018500                                                                          
018600     IF REQU-IDKUNDNR-KEY NOT = ALL '+'                                   
018710       MOVE REQU-IDKUNDNR-KEY TO WS-IDKUNDNR                              
018800     ELSE                                                                 
018910       MOVE ZERO              TO WS-IDKUNDNR                              
019100     END-IF                                                               
019200                                                                          
019210     IF REQU-IDDISTR-KEY  NOT = ALL '+'                                   
019221       MOVE REQU-IDDISTR-KEY  TO WS-IDDISTR                               
019230     ELSE                                                                 
019231       MOVE ZERO            TO WS-IDDISTR                                 
019260     END-IF                                                               
019270                                                                          
019280     IF REQU-IDORDNR-KEY  NOT = ALL '+'                                   
019291       MOVE REQU-IDORDNR-KEY  TO WS-IDORDNR                               
019292     ELSE                                                                 
019294       MOVE ZERO              TO WS-IDORDNR                               
019295     END-IF                                                               
019313                                                                          
019314***  VILKEN MARKERING FRÅN ANNULLATION GÄLLER ***                         
019318      MOVE REQU-FLJANEJ-IN  TO WS-FLJANEJ                                 
019320     .                                                                    
019321     EJECT                                                                
019322 B-KOLLA-INPUT SECTION.                                                   
019323                                                                          
019324     MOVE ZERO   TO NYCKEL-ID                                             
019325                                                                          
019326     IF WS-IDPRODNR NOT NUMERIC                                           
019328       MOVE ZERO TO WS-IDPRODNR                                           
019329       MOVE 19 TO WS-KDFEL                                                
019330     END-IF                                                               
019331                                                                          
019332     IF WS-IDKUNDNR NOT NUMERIC                                           
019333       MOVE ZERO TO WS-IDKUNDNR                                           
019334       MOVE 19 TO WS-KDFEL                                                
019335     END-IF                                                               
019336                                                                          
019337     IF WS-IDDISTR NOT NUMERIC                                            
019338       MOVE ZERO TO WS-IDDISTR                                            
019339       MOVE 19 TO WS-KDFEL                                                
019340     END-IF                                                               
019341                                                                          
019342     IF WS-IDORDNR NOT NUMERIC                                            
019343       MOVE ZERO TO WS-IDORDNR                                            
019344       MOVE 19 TO WS-KDFEL                                                
019345     END-IF                                                               
019346                                                                          
019348     MOVE REQU-IDDC-KEY      TO W-IDDC-ARTS                               
019352                                                                          
019353     IF REQU-IDPRODNR-KEY NOT = ALL '+'                                   
019355       MOVE ZERO TO     WS-IDDISTR                                        
019356                        WS-IDKUNDNR                                       
019357                        WS-IDORDNR                                        
019358       MOVE 1    TO     NYCKEL-ID                                         
019359     ELSE                                                                 
019360       IF REQU-IDDISTR-KEY NOT = ALL '+'                                  
019361       OR REQU-IDKUNDNR-KEY NOT = ALL '+'                                 
019362       OR REQU-IDORDNR-KEY NOT = ALL '+'                                  
019363         MOVE ZERO TO     WS-IDPRODNR                                     
019364         MOVE 2    TO     NYCKEL-ID                                       
019365       ELSE                                                               
019366         IF WS-IDPRODNR NOT = ZERO                                        
019367           MOVE ZERO   TO   WS-IDDISTR                                    
019368                            WS-IDKUNDNR                                   
019369                            WS-IDORDNR                                    
019370           MOVE 1      TO   NYCKEL-ID                                     
019371         ELSE                                                             
019372           MOVE ZERO   TO   WS-IDPRODNR                                   
019373           MOVE 2      TO   NYCKEL-ID                                     
019374         END-IF                                                           
019375       END-IF                                                             
019376     END-IF                                                               
019377                                                                          
019378     IF REQU-IDRADNR-IN NOT = ALL '+'                                     
019379       IF REQU-IDRADNR-IN NUMERIC                                         
019381         MOVE REQU-IDRADNR-IN TO WS-IDRADNR                               
019382       ELSE                                                               
019383         MOVE ZERO TO WS-IDRADNR                                          
019385         MOVE 22  TO WS-KDFEL                                             
019388       END-IF                                                             
019389     ELSE                                                                 
019391       MOVE 23  TO WS-KDFEL                                               
019394     END-IF                                                               
019395                                                                          
019396     IF REQU-IDARTNR-IN NOT = ALL '+'                                     
019397       IF REQU-IDARTNR-IN NUMERIC                                         
019399         MOVE REQU-IDARTNR-IN TO WS-IDARTNR                               
019400                                ALT-IDARTNR-IN                            
019401         MOVE IDARTNR-WS TO     TEST-IDARTNR                              
019402         IF BYT09-OBJEKT                                                  
019403            MOVE 13 TO WS-KDFEL                                           
019404         END-IF                                                           
019405       ELSE                                                               
019406         MOVE ZERO  TO WS-IDARTNR                                         
019408         MOVE 17  TO WS-KDFEL                                             
019411       END-IF                                                             
019412     ELSE                                                                 
019414       MOVE 14  TO WS-KDFEL                                               
019417     END-IF                                                               
019418                                                                          
019426        MOVE REQU-IDPW-IN TO IDPW-WS                                      
019428                                                                          
019429     PERFORM S01-KOLLA-TILLAATEN-PASSWORD                                 
019430     IF WS-PASSWORD-OK = NEJ                                              
019431         IF WS-KDFEL = ZERO                                               
019432             MOVE 12 TO WS-KDFEL                                          
019433         END-IF                                                           
019434     ELSE                                                                 
019435         MOVE IDPW-WS TO ALT-IDPW-IN                                      
019436     END-IF                                                               
019437                                                                          
019438     MOVE IDPW-WS TO RESP-IDPW-UT                                         
019439                                                                          
019440***FÖR RÄTT VÄRDE TILL IN- RESP UTFÄLTEN ***                              
019441     IF WS-FLJANEJ = '+' OR SPACE                                         
019442       MOVE 'N' TO WS-FLJANEJ                                             
019443     END-IF                                                               
019444     IF WS-FLJANEJ = 'J' OR 'N'                                           
019446       MOVE WS-FLJANEJ TO RESP-FLJANEJ-UT                                 
019447     ELSE                                                                 
019448       MOVE WS-FLJANEJ TO RESP-FLJANEJ-UT                                 
019452       MOVE 24 TO WS-KDFEL                                                
019453     END-IF                                                               
019454                                                                          
019455     IF WS-PASSWORD-OK = NEJ                                              
019456       MOVE ' '             TO RESP-FLJANEJ-UT                            
019457     END-IF                                                               
019458                                                                          
019463     EJECT                                                                
019464     IF REQU-IDANSTNR-KEY NUMERIC                                         
019470      MOVE REQU-IDANSTNR-KEY TO RESP-IDANSTNR-KEY                         
019471      INSPECT RESP-IDANSTNR-KEY REPLACING                                 
019472                             LEADING ZEROES BY SPACE                      
019473     END-IF                                                               
019474     IF WS-IDDISTR NUMERIC                                                
019475                                                                          
019476      MOVE WS-IDDISTR   TO RESP-IDDISTR-KEY                               
019477      INSPECT RESP-IDDISTR-KEY REPLACING                                  
019478                            LEADING ZEROES BY SPACE                       
019479     END-IF                                                               
019480     IF WS-IDKUNDNR NUMERIC                                               
019481       MOVE WS-IDKUNDNR  TO RESP-IDKUNDNR-KEY                             
019482       INSPECT RESP-IDKUNDNR-KEY REPLACING                                
019483                             LEADING ZEROES BY SPACE                      
019484     END-IF                                                               
019485     IF WS-IDORDNR NUMERIC                                                
019486      MOVE WS-IDORDNR   TO RESP-IDORDNR-KEY                               
019487      INSPECT RESP-IDORDNR-KEY REPLACING                                  
019488                            LEADING ZEROES BY SPACE                       
019489     END-IF                                                               
019490     IF WS-IDPRODNR NUMERIC                                               
019491      MOVE WS-IDPRODNR  TO RESP-IDPRODNR-KEY                              
019492      INSPECT RESP-IDPRODNR-KEY REPLACING                                 
019493                             LEADING ZEROES BY SPACE                      
019494     END-IF                                                               
019495     MOVE REQU-IDDC-KEY TO RESP-IDDC-KEY                                  
019496                                                                          
019497     MOVE WS-IDRADNR    TO RESP-IDRADNR-UT                                
019498     INSPECT RESP-IDRADNR-UT REPLACING                                    
019499                            LEADING ZEROES BY SPACE                       
019500     MOVE WS-IDARTNR   TO RESP-IDARTNR-UT                                 
019501     INSPECT  RESP-IDARTNR-UT REPLACING                                   
019502                            LEADING ZEROES BY SPACE                       
019503     IF REQU-IDKOLLI-KEY NUMERIC                                          
019506      MOVE REQU-IDKOLLI-KEY TO RESP-IDKOLLI-KEY                           
019507      INSPECT RESP-IDKOLLI-KEY REPLACING                                  
019508                            LEADING ZEROES BY SPACE                       
019509     END-IF                                                               
019510     .                                                                    
019511     EJECT                                                                
019512 C-KONTROLL-AV-ORDERDEL                  SECTION.                         
019513                                                                          
019514     MOVE NEJ                            TO WS-RAD-FUNNEN                 
019515     PERFORM CA-HAMTA-RAD-OCH-NYCKLAR                                     
019516                                                                          
019517     IF WS-RAD-FUNNEN = JA     AND                                        
019518        WS-KDFEL      = 0                                                 
019519       MOVE IDPRODNR-WS TO W-WDE601-IDPRODNR                              
019520       PERFORM CB-LAS-WDE601                                              
019521                                                                          
019522       IF VORD-KDORDSTA < 4                                               
019523         PERFORM CC-KONTROLLERA-RAD                                       
019524       ELSE                                                               
019525         MOVE 10                         TO WS-KDFEL                      
019526       END-IF                                                             
019527     ELSE                                                                 
019528       IF WS-RAD-FUNNEN = JA                                              
019529           CONTINUE                                                       
019530       ELSE                                                               
019531           MOVE 4                        TO WS-KDFEL                      
019532       END-IF                                                             
019533     END-IF                                                               
019534     .                                                                    
019535     EJECT                                                                
019536 CA-HAMTA-RAD-OCH-NYCKLAR                SECTION.                         
019537                                                                          
019538     IF NYCKEL-IDPRODNR                                                   
019539       PERFORM CAA-HAMTA-DIST-KUND-ORD-PLKLST                             
019540     ELSE                                                                 
019541       PERFORM CAB-HAMTA-PRODNR-PLKLST                                    
019542     END-IF                                                               
019543     .                                                                    
019544     EJECT                                                                
019545 CAA-HAMTA-DIST-KUND-ORD-PLKLST          SECTION.                         
019546                                                                          
019547     MOVE IDPRODNR-WS                    TO W-420-IDPRODNR-MIN            
019548                                            W-420-IDPRODNR-MAX            
019549     MOVE 0                              TO W-420-IDPURAD-MIN             
019550     MOVE 99999                          TO W-420-IDPURAD-MAX             
019551     PERFORM IMS-GU-WDE411-MED-IDPRODNR                                   
019552                                                                          
019553     IF SEGMENT-FINNS                                                     
019554       MOVE IDRADNR-WS                   TO W-420-IDPURAD-MIN             
019555       PERFORM IMS-GU-WDE411-BSEQ                                         
019556                                                                          
019557       IF SEGMENT-FINNS                                                   
019558         MOVE JA                         TO WS-RAD-FUNNEN                 
019559         MOVE ORAD-WDE411                TO SPAR-ORAD-WDE411              
019560         PERFORM IMS-GNP-WDE411-BSEQ                                      
019561         MOVE KORD-IDDISTR               TO IDDISTR-WS                    
019562         MOVE KORD-IDKUNDNR              TO IDKUNDNR-WS                   
019563         MOVE KORD-IDORDNR5              TO IDORDNR-WS                    
019564         MOVE KORD-IDPLKLST              TO IDPLKLST-WS                   
019565         MOVE KORD-IDPRODNR              TO IDPRODNR-WS                   
019566                                                                          
019567         IF KORD-IDUSER = '00000000'                                      
019568           MOVE 2                        TO WS-KDFEL                      
019569         END-IF                                                           
019570       END-IF                                                             
019571     ELSE                                                                 
019572       MOVE 1                            TO WS-KDFEL                      
019573     END-IF                                                               
019574     .                                                                    
019575     EJECT                                                                
019576 CAB-HAMTA-PRODNR-PLKLST                 SECTION.                         
019577                                                                          
019578     MOVE IDDISTR-WS                     TO W-4A1-IDDISTR                 
019579     MOVE IDKUNDNR-WS                    TO W-4A1-IDKUNDNR                
019580     MOVE IDORDNR-WS                     TO W-4A1-IDORDNR                 
019581     PERFORM IMS-GU-WDE401-ASEQ                                           
019582                                                                          
019583     IF SEGMENT-FINNS                                                     
019584       PERFORM UNTIL WS-RAD-FUNNEN = JA   OR                              
019585                     SEGMENT-SAKNAS       OR                              
019586                     SLUT-PA-BASEN                                        
019587         PERFORM CABA-LAS-WDE411                                          
019588                                                                          
019589         IF WS-OGAE12-FINNS = JA                                          
019590           MOVE JA                       TO WS-RAD-FUNNEN                 
019591           PERFORM CABB-BEHANDLA-RAD                                      
019592         ELSE                                                             
019593           PERFORM IMS-GN-WDE401-ASEQ                                     
019594         END-IF                                                           
019595       END-PERFORM                                                        
019596     ELSE                                                                 
019597       MOVE 1                            TO WS-KDFEL                      
019598     END-IF                                                               
019599     .                                                                    
019600     EJECT                                                                
019601 CABA-LAS-WDE411                         SECTION.                         
019602                                                                          
019603     MOVE KORD-IDUSER                    TO SPAR-IDUSER                   
019604     MOVE KORD-IDDISTR                   TO W-401-IDDISTR                 
019605     MOVE KORD-IDKUNDNR                  TO W-401-IDKUNDNR                
019606     MOVE KORD-IDKUNDRF                  TO W-401-IDKUNDRF                
019607     MOVE KORD-IDPRODNR                  TO W-401-IDPRODNR                
019608                                            SPAR-IDPRODNR                 
019609     MOVE KORD-IDPLKLST                  TO W-401-IDPLKLST                
019610                                            SPAR-IDPLKLST                 
019611     MOVE IDRADNR-WS                     TO W-420-IDPURAD                 
019612     MOVE KORD-IDDC                      TO WS-KORD-IDDC                  
019613     PERFORM IMS-GU-WDE411                                                
019614                                                                          
019615     IF SEGMENT-FINNS AND                                                 
019617        WS-KORD-IDDC      = REQU-IDDC-KEY                                 
019618                                                                          
019619       MOVE JA                           TO WS-OGAE12-FINNS               
019620     ELSE                                                                 
019621       MOVE NEJ                          TO WS-OGAE12-FINNS               
019622     END-IF                                                               
019623     .                                                                    
019624     EJECT                                                                
019625 CABB-BEHANDLA-RAD                       SECTION.                         
019626                                                                          
019627     MOVE SPAR-IDPRODNR                  TO IDPRODNR-WS                   
019628     MOVE SPAR-IDPLKLST                  TO IDPLKLST-WS                   
019629     MOVE ORAD-WDE411                    TO SPAR-ORAD-WDE411              
019630                                                                          
019631     IF SPAR-IDUSER = '00000000'                                          
019632       MOVE 2                            TO WS-KDFEL                      
019633     END-IF                                                               
019634     .                                                                    
019635     EJECT                                                                
019636 CB-LAS-WDE601                           SECTION.                         
019637                                                                          
019638     MOVE IDDISTR-WS                     TO W-401-IDDISTR                 
019639     MOVE IDKUNDNR-WS                    TO W-401-IDKUNDNR                
019640     MOVE IDORDNR-WS                     TO W-401-IDORDNR                 
019641     MOVE IDPRODNR-WS                    TO W-401-IDPRODNR                
019642     MOVE IDPLKLST-WS                    TO W-401-IDPLKLST                
019643     PERFORM IMS-GU-WDE601                                                
019644     .                                                                    
019645     EJECT                                                                
019646 CC-KONTROLLERA-RAD                      SECTION.                         
019647                                                                          
019648     EVALUATE TRUE                                                        
019649       WHEN SPAR-ORAD-KVLEVART > ZERO                                     
019650         MOVE 6                          TO WS-KDFEL                      
019651                                                                          
019652       WHEN SPAR-ORAD-FLNOLLJ = JA                                        
019653         MOVE 11                         TO WS-KDFEL                      
019654                                                                          
019655       WHEN SPAR-ORAD-KDRADSTA > 3                                        
019656         MOVE 5                          TO WS-KDFEL                      
019657                                                                          
019658       WHEN SPAR-ORAD-IDARTNR NOT = IDARTNR-WS                            
019661         MOVE 7                          TO WS-KDFEL                      
019662     END-EVALUATE                                                         
019663     .                                                                    
019664     EJECT                                                                
019665 F-UPPDAT-UTREDN-SALDO SECTION.                                           
019666                                                                          
019667     MOVE IDARTNR-WS             TO W-IDARTNR                             
019681     MOVE REQU-IDDC-KEY          TO W-IDDC-ARTS                           
019682     PERFORM IMS-GU-ARTS11                                                
019683     IF SEGMENT-FINNS                                                     
019684        IF SLAG-KVUTRS = ZERO                                             
019685           MOVE IDPRODNR-WS       TO ALT-IDPRODNR                         
019686           MOVE SPAR-ORAD-KDORDKL TO ALT-KDORDKL                          
019687           MOVE REQU-IDDC-KEY     TO ALT-IDDC-IN                          
019688           PERFORM IMS-INSERT-ALT                                         
019689        END-IF                                                            
019690     ELSE                                                                 
019691        MOVE 8 TO WS-KDFEL                                                
019692     END-IF                                                               
019695     .                                                                    
019696                                                                          
019697     EJECT                                                                
019698 H-UPPDAT-NOLLJ-FLAGGA SECTION.                                           
019699                                                                          
019700     MOVE IDRADNR-WS                     TO W-420-IDPURAD                 
019701     PERFORM IMS-GHU-WDE411                                               
019702     MOVE JA                 TO ORAD-FLNOLLJ                              
019703     PERFORM IMS-REPL-WDE411                                              
019705     IF WS-FLJANEJ = 'J'                                                  
019706        MOVE 15 TO WS-KDFEL                                               
019707     ELSE                                                                 
019711        MOVE 32 TO WS-KDFEL                                               
019712     END-IF                                                               
019713     .                                                                    
019714     EJECT                                                                
019715 J-HAMTA-MEDDELANDE SECTION.                                              
019716     SKIP2                                                                
019717                                                                          
019718     EVALUATE WS-KDFEL                                                    
019719         WHEN 1 MOVE FEL-1              TO RESP-IDMSG-ERROR               
019720                MOVE FEL-IDORDNR        TO RESP-IDELMT-ERROR              
019721         WHEN 2 MOVE FEL-2              TO RESP-IDMSG-ERROR               
019722         WHEN 3 MOVE FEL-3              TO RESP-IDMSG-ERROR               
019723         WHEN 4 MOVE FEL-4              TO RESP-IDMSG-ERROR               
019724                MOVE FEL-IDRADNR        TO RESP-IDELMT-ERROR              
019725         WHEN 5 MOVE FEL-5              TO RESP-IDMSG-ERROR               
019726         WHEN 6 MOVE FEL-6              TO RESP-IDMSG-ERROR               
019727         WHEN 7 MOVE FEL-7              TO RESP-IDMSG-ERROR               
019728                MOVE FEL-IDARTNR        TO RESP-IDELMT-ERROR              
019729         WHEN 8 MOVE FEL-8              TO RESP-IDMSG-ERROR               
019730                MOVE FEL-IDARTNR        TO RESP-IDELMT-ERROR              
019731         WHEN 9 MOVE FEL-9              TO RESP-IDMSG-ERROR               
019732         WHEN 10 MOVE FEL-10            TO RESP-IDMSG-ERROR               
019733         WHEN 11 MOVE FEL-11            TO RESP-IDMSG-ERROR               
019734         WHEN 12 MOVE FEL-12            TO RESP-IDMSG-ERROR               
019735         WHEN 13 MOVE FEL-13            TO RESP-IDMSG-ERROR               
019736         WHEN 14 MOVE FEL-26            TO RESP-IDMSG-ERROR               
019737                 MOVE FEL-IDARTNR       TO RESP-IDELMT-ERROR              
019738*** FÖR TEXT ANNULERING UTFÖRD ***                                        
019739         WHEN 15 MOVE MED-2             TO RESP-IDMSG-INFO                
019740         WHEN 17 MOVE FEL-IDARTNR       TO RESP-IDELMT-ERROR              
019741                 MOVE FEL-27            TO RESP-IDMSG-ERROR               
019742         WHEN 18 MOVE FEL-18            TO RESP-IDMSG-ERROR               
019743         WHEN 19 MOVE FEL-19            TO RESP-IDMSG-ERROR               
019744         WHEN 32 MOVE MED-1             TO RESP-IDMSG-INFO                
019745         WHEN 21 MOVE FEL-21            TO RESP-IDMSG-ERROR               
019746         WHEN 22 MOVE FEL-27            TO RESP-IDMSG-ERROR               
019747                 MOVE FEL-IDRADNR       TO RESP-IDELMT-ERROR              
019748         WHEN 23 MOVE FEL-IDRADNR       TO RESP-IDELMT-ERROR              
019749                 MOVE FEL-26            TO RESP-IDMSG-ERROR               
019750         WHEN 24 MOVE FEL-FLJANEJ       TO RESP-IDELMT-ERROR              
019751                 MOVE FEL-7             TO RESP-IDMSG-ERROR               
019752     END-EVALUATE                                                         
019753     .                                                                    
019754     EJECT                                                                
019755 K-VISA-BILD-IGEN SECTION.                                                
019758     MOVE ZERO              TO  RESP-IDARTNR-UT                           
019759                                RESP-IDRADNR-UT                           
019760     MOVE SPACE             TO  RESP-IDPW-UT                              
019761                                RESP-FLJANEJ-UT                           
019762     IF REQU-IDARTNR-IN   NUMERIC                                         
019763      MOVE REQU-IDARTNR-IN   TO  RESP-IDARTNR-IN                          
019764     END-IF                                                               
019765     IF RESP-IDRADNR-IN  NUMERIC                                          
019766      MOVE REQU-IDRADNR-IN   TO  RESP-IDRADNR-IN                          
019767     END-IF                                                               
019768     MOVE REQU-IDPW-IN      TO  RESP-IDPW-IN                              
019769     MOVE REQU-FLJANEJ-IN   TO  RESP-FLJANEJ-IN                           
019770     .                                                                    
019771     EJECT                                                                
019798                                                                          
019799 S01-KOLLA-TILLAATEN-PASSWORD SECTION.                                    
019800     SKIP2                                                                
019801                                                                          
019802     MOVE WS-IDUSER     TO SEC-IDUSER                                     
019803     MOVE WS-EGEN-BILD  TO SEC-IDTRANS                                    
019804     MOVE IDPW-WS       TO SEC-IDKEY                                      
019805     CALL WSECURIT USING                                                  
019806          SEC-IDUSER                                                      
019807          SEC-IDTRANS                                                     
019808          SEC-IDKEY                                                       
019809          SEC-KDSVAR                                                      
019810                                                                          
019811     IF SEC-KDSVAR = SPACE                                                
019812         MOVE JA TO WS-PASSWORD-OK                                        
019813     ELSE                                                                 
019814         MOVE NEJ TO WS-PASSWORD-OK                                       
019815     END-IF                                                               
019816     .                                                                    
019817     EJECT                                                                
019818                                                                          
019819*    --- DISPATCHER SECTIONS                                              
019820 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
019821                                                                          
019822     MOVE 'GETARG'               TO SUB-KDFUNC                            
019823     MOVE 'CARPARTS.LDC.CONFIRMZEROES'      TO SUB-ADDISPABS              
019830     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
019900                                                                          
020000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
020100                                                                          
020200     IF SUB-KDRC > 0                                                      
020300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
020400       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
020500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
020600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020700     END-IF                                                               
020800     .                                                                    
020900     SKIP3                                                                
021000 S02-RETURN-RESPONSE SECTION.                                             
021100                                                                          
021200     MOVE 'RETURN'                   TO SUB-KDFUNC                        
021300     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
021400                                                                          
021500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
021600                                                                          
021700     IF SUB-KDRC > 0                                                      
021800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
021900       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
022000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
022100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022200     END-IF                                                               
022201     .                                                                    
022202     EJECT                                                                
022210* IMS SEKTIONER                                                           
022220     SKIP2                                                                
022230 IMS-INSERT-ALT SECTION.                                                  
022240                                                                          
022250     MOVE LOW-VALUE TO ALT-Z1 ALT-Z2                                      
022260     MOVE SPACE TO GODK-STATUSKODER                                       
022270     CALL CBLTDLI USING ISRT ALT-PCB ALT-IO-AREA                          
022280     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
022290     PERFORM IMS-STATUSKONTROLL                                           
022291     .                                                                    
022292     SKIP2                                                                
022293 IMS-GU-WDE411  SECTION.                                                  
022294                                                                          
022295     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
022296            DELIMITED BY SIZE INTO SSA1                                   
022297     STRING 'WDE411  (IDPURAD  =' W-WDE420-IDPURAD-X ')'                  
022298            DELIMITED BY SIZE INTO SSA2                                   
022299     MOVE '  GE' TO GODK-STATUSKODER                                      
022300     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-AREA SSA1 SSA2                 
022301     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
022302     PERFORM IMS-STATUSKONTROLL                                           
022303     .                                                                    
022304     SKIP3                                                                
022305 IMS-GHU-WDE411 SECTION.                                                  
022306                                                                          
022307     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
022308            DELIMITED BY SIZE INTO SSA1                                   
022309     STRING 'WDE411  (IDPURAD  =' W-WDE420-IDPURAD-X ')'                  
022310            DELIMITED BY SIZE INTO SSA2                                   
022311     MOVE '  ' TO GODK-STATUSKODER                                        
022312     CALL CBLTDLI USING GHU WDE4-PCB DLI-IO-AREA SSA1 SSA2                
022313     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
022314     PERFORM IMS-STATUSKONTROLL                                           
022315     .                                                                    
022316     SKIP3                                                                
022317 IMS-GU-WDE601   SECTION.                                                 
022318                                                                          
022319     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
022320            DELIMITED BY SIZE INTO SSA1                                   
022321     MOVE '  ' TO GODK-STATUSKODER                                        
022322     CALL CBLTDLI USING GU  WDE6-PCB DLI-IO-WDE601 SSA1                   
022323     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
022324     PERFORM IMS-STATUSKONTROLL                                           
022325     .                                                                    
022326     SKIP3                                                                
022327 IMS-REPL-WDE411                         SECTION.                         
022328     SKIP2                                                                
022329     MOVE '    ' TO GODK-STATUSKODER                                      
022330     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-AREA                         
022331     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
022332     PERFORM IMS-STATUSKONTROLL                                           
022333     .                                                                    
022334     EJECT                                                                
022335 IMS-GU-WDE401-ASEQ                      SECTION.                         
022336                                                                          
022337     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
022338            DELIMITED BY SIZE INTO SSA1                                   
022339     MOVE '  GE' TO GODK-STATUSKODER                                      
022340     CALL CBLTDLI USING GU WDE4A-PCB DLI-IO-AREA SSA1                     
022341     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
022342     PERFORM IMS-STATUSKONTROLL                                           
022343     .                                                                    
022344     SKIP3                                                                
022345 IMS-GN-WDE401-ASEQ                      SECTION.                         
022346                                                                          
022347     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
022348            DELIMITED BY SIZE INTO SSA1                                   
022349     MOVE '  GEGB' TO GODK-STATUSKODER                                    
022350     CALL CBLTDLI USING GN WDE4A-PCB DLI-IO-AREA SSA1                     
022351     MOVE WDE4A-STATUS-CODE TO STATUS-WS                                  
022352     PERFORM IMS-STATUSKONTROLL                                           
022353     .                                                                    
022354     EJECT                                                                
022355 IMS-GU-WDE411-MED-IDPRODNR              SECTION.                         
022356                                                                          
022357     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X                    
022358                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
022359            DELIMITED BY SIZE INTO SSA1                                   
022360     MOVE '  GE' TO GODK-STATUSKODER                                      
022361     CALL CBLTDLI USING GU WDE4B-PCB DLI-IO-AREA SSA1                     
022362     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
022363     PERFORM IMS-STATUSKONTROLL                                           
022364     .                                                                    
022365     SKIP3                                                                
022366 IMS-GU-WDE411-BSEQ                      SECTION.                         
022367                                                                          
022368     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-MIN-X ')'                
022369            DELIMITED BY SIZE INTO SSA1                                   
022370     MOVE '  GE' TO GODK-STATUSKODER                                      
022371     CALL CBLTDLI USING GU WDE4B-PCB DLI-IO-AREA SSA1                     
022372     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
022373     PERFORM IMS-STATUSKONTROLL                                           
022374     .                                                                    
022375     SKIP3                                                                
022376 IMS-GNP-WDE411-BSEQ                     SECTION.                         
022377                                                                          
022378     STRING 'WDE411  (WDE4BSEQ =' W-WDE4B-KEYSEQ-MIN-X ')'                
022379            DELIMITED BY SIZE INTO SSA1                                   
022380     MOVE 'WDE401   ' TO SSA2                                             
022381     MOVE '  ' TO GODK-STATUSKODER                                        
022382     CALL CBLTDLI USING GNP WDE4B-PCB DLI-IO-AREA SSA1 SSA2               
022383     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
022384     PERFORM IMS-STATUSKONTROLL                                           
022385     .                                                                    
022386     EJECT                                                                
022399 IMS-GU-ARTS11        SECTION.                                            
022400                                                                          
022401     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
022402            DELIMITED BY SIZE INTO SSA1                                   
022403     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
022404            DELIMITED BY SIZE INTO SSA2                                   
022405     MOVE '  GE' TO GODK-STATUSKODER                                      
022406     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA2 SSA1 SSA2                
022407     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
022408     PERFORM IMS-STATUSKONTROLL                                           
022409     .                                                                    
022410     EJECT                                                                
022411 IMS-STATUSKONTROLL SECTION.                                              
022412     SKIP2                                                                
022413     SET STATUS-IX TO 1                                                   
022414     SEARCH GODK-STATUS                                                   
022415       AT END                                                             
022416         CALL FELLOG                                                      
022417       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022418         CONTINUE                                                         
022419     END-SEARCH                                                           
022420     .                                                                    
