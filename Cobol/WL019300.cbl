000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL019300.                                                
000300 AUTHOR.         BERT ANDERSSON.                                          
000400 DATE-WRITTEN.   06/12/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.SHIPPINGPRINTBOLLATOTAL'                   
000908*                                                                         
001008*    FUNKTION:                                                            
001108*        SKAPAR PRELIMINÄR OCH TOTALLISTA FÖR ITALIEN.                    
001208*        BOLLA.                                                           
001308*                                                                         
001408*OBS! SKAPAR LISTA VIA D & P.                                             
001508*                                                                         
001608******************************************************************        
001708*        WL019300 PROGRAM IS A REPLICA OF W4069700 PROGRAM                
001808*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001908******************************************************************        
002008*                                                                         
002108*        PROGRAMMET LÄSER      WL4491 (WDR4)                              
002208*                              WLGMTA (WDB2)                              
002308*                                                                         
002408*                                                                         
002508*    INDATA.                                                              
002608*        TRANSACTION: WL0192T                                             
002708*        REQUEST:     WL0192I1                                            
002808*                                                                         
002908*    OUTDATA.                                                             
003008*        RESPONSE:    WL0192O1                                            
003108                                                                          
003208     SKIP3                                                                
003308 ENVIRONMENT DIVISION.                                                    
003408     EJECT                                                                
003508 DATA DIVISION.                                                           
003608 WORKING-STORAGE SECTION.                                                 
003708                                                                          
003808*    -- CHECKED BY WY2000                                                 
003908 77  IDPGM                       PIC X(08)   VALUE 'WL019300'.            
004008                                                                          
004108*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004208 77  FILLER                      PIC X(08)   VALUE 'FELTEXT:'.            
004308 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004408 77  FILLER                      PIC X(08)   VALUE 'PGM-POS:'.            
004508 77  WS-PGM-POS                  PIC X(40)   VALUE SPACE.                 
004608 77  FILLER                      PIC X(08)   VALUE 'S90-POS:'.            
004708 77  WS-S90-POS                  PIC X(40)   VALUE SPACE.                 
004808 77  FILLER                      PIC X(08)   VALUE 'S90OPCL:'.            
004908 77  WS-S90-OPCL                 PIC X(40)   VALUE SPACE.                 
005008 77  FILLER                      PIC X(08)   VALUE 'IMS-SEC:'.            
005108 77  WS-IMS-SEC                  PIC X(40)   VALUE SPACE.                 
005208                                                                          
005508 77  KDRC-DISPLAY                PIC Z(5).                                
005608                                                                          
005708 77  JA                         PIC X          VALUE 'J'.                 
005808 77  NEJ                        PIC X          VALUE 'N'.                 
005908 77  INDX                       PIC S9(9)     VALUE +0 COMP SYNC.         
006008 77  MAX-INDX                   PIC S9(9)     VALUE +10 COMP SYNC.        
006108 77  SPAR-IDZON                 PIC X(2)       VALUE SPACE.               
006208 77  SPAR-IDKUNDNR              PIC S9(7)      VALUE ZERO COMP-3.         
006308 77  WS-IDKUNDNR                PIC S9(7)      VALUE ZERO COMP-3.         
006408                                                                          
006508 77  WS-IDTRPTNR                PIC S9(3)      VALUE ZERO COMP-3.         
006608 77  WS-KDFRAKT                 PIC S9(3)      VALUE ZERO COMP-3.         
006708                                                                          
006808 77  FILLER                     PIC X(8)       VALUE 'AAAAAAAA'.          
006908 77  WS-IDTRPBOR                PIC S9(5)      VALUE ZERO COMP-3.         
007008 77  WS-ANTAL-KUND              PIC S9(3)      VALUE ZERO COMP-3.         
007108 77  WS-ANTAL-BOLLA             PIC S9(3)      VALUE ZERO COMP-3.         
007208 77  WS-ANTAL-ORDER             PIC S9(3)      VALUE ZERO COMP-3.         
007308 77  WS-SUM-KUND-KOLLI          PIC S9(5)      VALUE ZERO COMP-3.         
007408 77  WS-SUM-KUND-VIKT           PIC S9(6)V9(1) VALUE ZERO COMP-3.         
007508 77  WS-SUM-KUND-VOLYM          PIC S9(4)V9(3) VALUE ZERO COMP-3.         
007608 77  FILLER                     PIC X(8)       VALUE 'BBBBBBBB'.          
007708 77  WS-SUM-TOT-KOLLI           PIC S9(5)      VALUE ZERO COMP-3.         
007808 77  WS-SUM-TOT-VIKT            PIC S9(6)V9(1) VALUE ZERO COMP-3.         
007908 77  WS-SUM-TOT-VOLYM           PIC S9(4)V9(3) VALUE ZERO COMP-3.         
008008 77  FILLER                     PIC X(8)       VALUE 'B2B2B2B2'.          
008108 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
008208 77  WS-IDMSG-INFO               PIC X(3).                                
008308 77  WS-IDELMT-ERROR             PIC X(16).                               
008408 77  WS-IDMSG-ERROR              PIC X(3).                                
008508                                                                          
008608 01  WS-YYMMDDHHMM.                                                       
008708     03 WS-YYMMDD                PIC  9(6).                               
008808     03 WS-TIME                  PIC  9(4).                               
008908                                                                          
009008 01  WS-HHMMSSTH                 PIC  9(8).                               
009108 01  FILLER REDEFINES WS-HHMMSSTH.                                        
009208       03  WS-HHMM               PIC 9(4).                                
009308       03  WS-SSTH               PIC 9(4).                                
009408                                                                          
009508 01  WS-BOLLANR                  PIC X(8)      VALUE SPACE.               
009608                                                                          
009708 01  FILLER REDEFINES WS-BOLLANR.                                         
009808     03  WS-IDTRPBOT             PIC X.                                   
009908     03  WS-IDTRPBON             PIC 9(7).                                
010008                                                                          
010108 01  VCOM-DATUM                  PIC 9(6).                                
010208                                                                          
010308 01  FILLER                     PIC X(8)       VALUE 'CCCCCCCC'.          
010408 01  DAGENS-DATUM.                                                        
010508     03  DAGENS-AAR              PIC 9(2).                                
010608     03  DAGENS-MAN              PIC 9(2).                                
010708     03  DAGENS-DAG              PIC 9(2).                                
010808                                                                          
010908*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
011008                                                                          
011108                                                                          
011208 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
011308     88  NYCKLAR-OK                          VALUE 'J'.                   
011408     88  NYCKLAR-FEL                         VALUE 'N'.                   
011508                                                                          
011608 77  KUND-SW                     PIC X       VALUE 'J'.                   
011708     88  NY-KUND                             VALUE 'J'.                   
011808     88  GAMMAL-KUND                         VALUE 'N'.                   
011908                                                                          
012008 77  ZONA-SW                     PIC X       VALUE 'J'.                   
012108     88  NY-ZONA                             VALUE 'J'.                   
012208     88  GAMMAL-ZONA                         VALUE 'N'.                   
012308                                                                          
012408 77  BOLLADOK-SW                 PIC X       VALUE 'J'.                   
012508     88  BOLLADOK-REFNR-NY                   VALUE 'J'.                   
012608     88  BOLLADOK-REFNR-OLD                  VALUE 'N'.                   
012708                                                                          
012808 77  VCOM-SW                     PIC X       VALUE 'J'.                   
012908     88  VCOM-SKRIVS                         VALUE 'J'.                   
013008     88  VCOM-SKRIVS-EJ                      VALUE 'N'.                   
013108                                                                          
013208 77  TRAFF-SW                    PIC X       VALUE 'J'.                   
013308     88  TRAFF                               VALUE 'J'.                   
013408     88  TRAFF-EJ                            VALUE 'N'.                   
013508                                                                          
013608 77  FILLER                     PIC X(8)       VALUE 'DDDDDDDD'.          
013708 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
013808     88  EGEN-MID                            VALUE 'L193'.                
013908     88  GODK-MID                            VALUE 'L192' 'L193'          
014008                                                   '4699'.                
014108     88  HELP-MID                            VALUE '0551'.                
014208     EJECT                                                                
014308*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
014408 01  GENERELLA-SUBPROGRAM.                                                
014508     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014708     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014808     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014908     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015008     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
015108     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
015208                                                                          
015308*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015408*01 -COPY WMEDAREA                                                        
015508     SKIP3                                                                
015608 01  MESSAGE-CODES.                                                       
015708     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
015808     03  SYSTEM-ERROR            PIC X(03)   VALUE '099'.                 
015908     EJECT                                                                
016008*    --- PARAMETERS TO ABEND                                              
016108                                                                          
016208 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
016308 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
016408 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
016508*                                                                         
016608 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
016708     SKIP3                                                                
016808*01  -COPY WZ01SUB                                                        
016908     SKIP3                                                                
017008 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
017108*01  -COPY WZ01SEND                                                       
017208*                                                                         
017308     SKIP3                                                                
017408 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
017508     SKIP3                                                                
017509 01  MSG-IO-AREA2.                                                        
017510   03  MSG-KVLL2                 PIC S9(4)   BINARY.                      
017520   03  MSG-KDZZ2                 PIC XX.                                  
017540   03  RESP-AREA.                                                         
017708*    05  -COPY WZ01RESP                                                   
017808                                                                          
017908     SKIP3                                                                
018008 01  FILLER                      PIC X(16) VALUE 'DOC-HEAD-AREA:'.        
018108 01  DOC-HEAD-AREA.                                                       
018208*    03  -COPY WL019311                                                   
018308     SKIP3                                                                
018408 01  FILLER                      PIC X(16) VALUE 'DOC-KUND-HEAD:'.        
018508 01  DOC-KUND-HEAD-AREA.                                                  
018608*    03  -COPY WL019321                                                   
018708     SKIP3                                                                
018808 01  FILLER                      PIC X(16) VALUE 'DOC-LINE-AREA:'.        
018908 01  DOC-LINE-AREA.                                                       
019008*    03  -COPY WL019322                                                   
019108     SKIP3                                                                
019208 01  FILLER                      PIC X(16) VALUE 'DOC-KUND-SUM :'.        
019308 01  DOC-KUND-SUM-AREA.                                                   
019408*    03  -COPY WL019323                                                   
019508     SKIP3                                                                
019608 01  FILLER                      PIC X(16) VALUE 'DOC-TRP-SUM  :'.        
019708 01  DOC-TRP-SUM-AREA.                                                    
019808*    03  -COPY WL019330                                                   
019908     SKIP3                                                                
020008 01  FILLER                      PIC X(16) VALUE 'DOC-BOLLA-TOT:'.        
020108 01  DOC-BOLLA-TOT-AREA.                                                  
020208*    03  -COPY WL019340                                                   
020308     SKIP3                                                                
020408 01  FILLER                      PIC X(16) VALUE 'HDR-AREA'.              
020508 01  HDR-AREA.                                                            
020608*    03  -COPY WZ01REQU  -PRE HDR-                                        
020708*    03  -COPY WZ04HDR                                                    
020808     SKIP3                                                                
020908*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
021008*                                                                         
021108 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
021208     SKIP3                                                                
021308*01  MID -COPY WL0193I1                                                   
021408     EJECT                                                                
021508 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
021608     SKIP3                                                                
021708*01  -COPY WMSGAREA                                                       
021808     EJECT                                                                
021908 01  FILLER                     PIC X(8)       VALUE 'LISTRAD:'.          
022008********  LIST RUBRIKER OCH RADER   ******************************        
022108 01  WS-LISTRAD.                                                          
022208     03  LIST-RAD              PIC X(132).                                
022308                                                                          
022408                                                                          
022508 01  FILLER                     PIC X(8)       VALUE 'VCOMLIST'.          
022608*01  LIST-RADER.                                                          
022708*    03 LISTRAD-VCOM.                                                     
022808*      05   COPY  WL0193IA -PRE VCOM-                                     
022908                                                                          
023008 01 LISTRAD-VCOM.                                                         
023108     03 VCOM-IDTRPBOR              PIC S9(5)  VALUE ZERO COMP-3.          
023208     03 VCOM-KDFRAKT               PIC S9(3)  VALUE ZERO COMP-3.          
023308     03 VCOM-IDZON                 PIC X(2)   VALUE SPACE.                
023408     03 VCOM-IDKUNDNR              PIC S9(7)  VALUE ZERO COMP-3.          
023508     03 VCOM-IDORDNR5              PIC 9(5)   VALUE ZERO.                 
023608     03 VCOM-TIORDREG              PIC S9(7)  VALUE ZERO COMP-3.          
023708     03 VCOM-KVKOLLI               PIC S9(5)  VALUE ZERO COMP-3.          
023808     03 VCOM-VKORDBTO          PIC S9(6)V9(1) VALUE ZERO COMP-3.          
023908     03 VCOM-VLORDBTO          PIC S9(4)V9(3) VALUE ZERO COMP-3.          
024008     03 VCOM-IDTRPBO               PIC X(8)   VALUE SPACE.                
024108     03 VCOM-TIREGDAT              PIC S9(7)  VALUE ZERO COMP-3.          
024208                                                                          
024308 01 LISTRAD-VCOM2.                                                        
024408     03 VCOM2-IDTRPBOR             PIC 9(5)   VALUE ZERO.                 
024508     03 VCOM2-KDFRAKT              PIC 9(2)   VALUE ZERO.                 
024608     03 VCOM2-IDZON                PIC X(2)   VALUE SPACE.                
024708     03 VCOM2-TIORDREG             PIC 9(6)   VALUE ZERO.                 
024808     03 VCOM2-IDTRPBO              PIC X(8)   VALUE SPACE.                
024908     03 VCOM2-TIREGDAT             PIC 9(6)   VALUE ZERO.                 
025008     03 VCOM2-IDDISTR              PIC 9(4)   VALUE ZERO.                 
025108     03 VCOM2-IDKUNDNR             PIC 9(6)   VALUE ZERO.                 
025208     03 VCOM2-IDORDNR7             PIC 9(7)   VALUE ZERO.                 
025308     03 VCOM2-IDKOLLI              PIC 9(5)   VALUE ZERO.                 
025408     03 VCOM2-VKORDBTO             PIC 9(6)V9(1)  VALUE ZERO.             
025508     03 VCOM2-VLORDBTO             PIC 9(4)V9(3)  VALUE ZERO.             
025608                                                                          
025708     EJECT                                                                
025808*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025908*                                                                         
026008     SKIP2                                                                
026108 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026208     SKIP3                                                                
026308 01  NYCKLAR-TILL-DLI.                                                    
026408                                                                          
026508     03  W-IDGMT-X.                                                       
026608         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
026708         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
026808                                                                          
026908     03  W-WDGXKEY-X.                                                     
027008         05  W-IDHTYP            PIC X(4)    VALUE '4491'.                
027108         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
027208         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
027308                                                                          
027408     03  W-IDLBBET-X.                                                     
027508         05  W-IDLBBET-SOEK      PIC X(12)   VALUE SPACE.                 
027608                                                                          
027900     03  W-IDPRODNR-X.                                                    
028000         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO  COMP-3.          
028100                                                                          
028800     03  W-TIAA-X.                                                        
028900         05  W-TIAA              PIC S9(3)   VALUE ZERO.                  
029000                                                                          
029100     03  W-KY4494-MIN-X.                                                  
029200         05  W-DALASTN-MIN       PIC  9(8)   VALUE ZERO.                  
029300         05  W-IDTRPTNR-MIN      PIC S9(3)   VALUE ZERO  COMP-3.          
029400         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
029500                                                                          
029600     03  W-KY4494-MAX-X.                                                  
029700         05  W-DALASTN-MAX       PIC  9(8)   VALUE ZERO.                  
029800         05  W-IDTRPTNR-MAX      PIC S9(3)   VALUE ZERO  COMP-3.          
029900         05  FILLER              PIC X(24)   VALUE HIGH-VALUE.            
030000                                                                          
030100*    --- STATUS-KOD FRÅN IMS                                              
030200 01  STATUS-WS                   PIC XX.                                  
030300     88  SEGMENT-FINNS                       VALUE '  '.                  
030400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
030500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030600     SKIP2                                                                
030700 01  GODK-STATUSKODER.                                                    
030800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030900     SKIP3                                                                
031000 01  SSA1                        PIC X(128).                              
031100 01  SSA2                        PIC X(64).                               
031200     EJECT                                                                
031300*    --- IMS FUNKTIONSKODER                                               
031400*01  -COPY W0003                                                          
031500     EJECT                                                                
031600*    ---  DLI INPUT-OUTPUT AREA                                           
031700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
031800     SKIP3                                                                
031900 01  DLI-IO-AREA.                                                         
032000     03  IO-AREA                 PIC X(1300)  VALUE SPACE.                
032100     SKIP3                                                                
032200     03  WL449111 REDEFINES IO-AREA.                                      
032300*        05  -COPY WDGX4492  -PRE BOLLA-                                  
032310                                                                          
032400     EJECT                                                                
032600 01  DLI-IO-AREA1.                                                        
032700     03  IO-AREA1                PIC X(300)  VALUE SPACE.                 
032800     SKIP3                                                                
032900     03  WL449112 REDEFINES IO-AREA1.                                     
033000*        05  -COPY WDGX4494  -PRE BOLLA-                                  
033010                                                                          
033100     EJECT                                                                
033300 01  DLI-IO-AREA2.                                                        
033400     03  WLGMTA01.                                                        
033500*        05  -COPY WDB201                                                 
033510                                                                          
033600     EJECT                                                                
033610 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E601'.         
033620 01  DLI-IO-E601.                                                         
033630*    03  -COPY WDE601                                                     
033631                                                                          
033640     EJECT                                                                
033650 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E611'.         
033660 01  DLI-IO-E611.                                                         
033670*    03  -COPY WDE611                                                     
033671                                                                          
033680     EJECT                                                                
033800 LINKAGE SECTION.                                                         
033900                                                                          
034000*01  -COPY W0009   -PRE MSG-                                              
034100     EJECT                                                                
034200 01  DISTRDOC-PCB                PIC X.                                   
034300                                                                          
034400 01  DISTRWEB-PCB                PIC X.                                   
034500     EJECT                                                                
034600*01  -COPY W0008  -PRE BOLLA-                                             
034700     05  FILLER                  PIC X.                                   
034800     EJECT                                                                
034900*01  -COPY W0008  -PRE GMTA-                                              
035000     05  FILLER                  PIC X.                                   
035100     EJECT                                                                
035110*01  -COPY W0008  -PRE WDE6-                                              
035120     05  FILLER                  PIC X.                                   
035130     EJECT                                                                
035200 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB DISTRWEB-PCB              
035300                           BOLLA-PCB GMTA-PCB WDE6-PCB.                   
035400 MAIN SECTION.                                                            
035500     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB DISTRWEB-PCB              
035600                           BOLLA-PCB GMTA-PCB WDE6-PCB.                   
035700                                                                          
035800     PERFORM IMS-GET-MSG                                                  
035801     IF SEGMENT-FINNS                                                     
036000       PERFORM A-INIT                                                     
036100       IF NYCKLAR-OK                                                      
036200         IF REQU-KDSVAR = 'P'                                             
036300           PERFORM C-LAES-SKRIV-PREL                                      
036400         ELSE                                                             
036502           IF REQU-KDSVAR = 'A'                                           
036600             PERFORM E-LAES-SKRIV-NY-VCOM                                 
036700           ELSE                                                           
036800             PERFORM D-LAES-SKRIV-TOTAL                                   
036900             IF BOLLADOK-REFNR-NY                                         
037000               PERFORM IMS-GHNP-FIRST-WL4492                              
037100               IF SEGMENT-FINNS                                           
037200                 ADD +1 TO BOLLA-4492-IDTRPBOR                            
037300                 PERFORM IMS-REPLACE-WL4492                               
037400               END-IF                                                     
037500             END-IF                                                       
037600           END-IF                                                         
037700         END-IF                                                           
037800       END-IF                                                             
037900                                                                          
038000     END-IF                                                               
038100                                                                          
038304     IF REQU-KDSVAR = 'A'                                                 
038305*      - VID 'P' OCH 'F' SKAPAS WEB-LISTOR, OCH DÅ ÄR DET                 
038306*      - D&P SOM RETURNERAR ETT SVAR, MEN VID 'A' GÖRS BARA               
038307*      - EN VCOM-SÄNDNING OCH DÅ MÅSTE DET HÄR PROGRAMMET                 
038308*      - SVARA DEN ANROPANDE WEBBEN.                                      
038309*      - TACK VARE ATT SVAREN SER OLIKA UT KAN WEBBEN AVGÖRA              
038310*      - VEM SOM SVARAR OCH HANTERA DET PÅ OLIKA SÄTT.                    
038404       PERFORM S02-RETURN-RESPONSE                                        
038504     END-IF                                                               
038600     MOVE ZERO TO RETURN-CODE                                             
038700     GOBACK                                                               
038800     .                                                                    
038900     EJECT                                                                
039000 A-INIT SECTION.                                                          
039100     MOVE 'STA A-INIT '     TO WS-PGM-POS                                 
039200                                                                          
039300     MOVE MSG-INDATA-MINUS-1-TRANSKOD    TO REQU-WL0193I1                 
039400     MOVE MSG-IDTRANS-1 TO W-IDTRANS                                      
039500                                                                          
039600     ACCEPT WS-YYMMDD      FROM DATE                                      
039700     ACCEPT WS-HHMMSSTH    FROM TIME                                      
039800     ACCEPT WS-TIME        FROM TIME                                      
039900                                                                          
040000     ACCEPT VCOM-DATUM     FROM DATE                                      
040100                                                                          
040200     MOVE REQU-IDDC        TO W-IDDC                                      
040300     .                                                                    
040400     EJECT                                                                
040500 C-LAES-SKRIV-PREL              SECTION.                                  
040600     MOVE 'STA C-LAES-SKRIV-PREL '     TO WS-PGM-POS                      
040700                                                                          
040800     MOVE +0 TO WS-ANTAL-KUND                                             
040900                WS-ANTAL-BOLLA                                            
041000                                                                          
041100     MOVE REQU-TIDATUM TO W-DALASTN-MIN                                   
041200                          W-DALASTN-MAX                                   
041300     IF REQU-TIDATUM NOT = ZERO                                           
041400       IF REQU-TIDATUM < 500000                                           
041500         MOVE 20       TO W-DALASTN-MIN (1:2)                             
041600                          W-DALASTN-MAX (1:2)                             
041700       ELSE                                                               
041800         IF REQU-TIDATUM < 999999                                         
041900           MOVE 19     TO W-DALASTN-MIN (1:2)                             
042000                          W-DALASTN-MAX (1:2)                             
042100         ELSE                                                             
042200           MOVE 99999999 TO W-DALASTN-MIN                                 
042300                            W-DALASTN-MAX                                 
042400         END-IF                                                           
042500       END-IF                                                             
042600     END-IF                                                               
042700                                                                          
042800     MOVE REQU-IDTRPTNR TO W-IDTRPTNR-MIN                                 
042900                          W-IDTRPTNR-MAX                                  
043000                          WS-IDTRPTNR                                     
043100                                                                          
043200     MOVE REQU-IDLBBET TO W-IDLBBET-SOEK                                  
043300                                                                          
043400     MOVE +0    TO SPAR-IDKUNDNR                                          
043500     MOVE NEJ   TO KUND-SW                                                
043600                   ZONA-SW                                                
043700     MOVE LOW-VALUE TO SPAR-IDZON                                         
043800     MOVE REQU-IDDC TO W-IDDC                                             
043900                       B11-IDDC                                           
044000                                                                          
044100     PERFORM IMS-GHU-WL4491                                               
044200     IF SEGMENT-FINNS                                                     
044300                                                                          
044400       PERFORM S90-OPEN-DAP-SEND-WEB                                      
044500       MOVE 001             TO HDR-REQU-IDMSGVER                          
044600       MOVE REQU-KDPGMACT   TO HDR-REQU-KDPGMACT                          
044700       MOVE REQU-IDUSER     TO HDR-REQU-IDUSER                            
044800                                                                          
044900*BOLLA-PARZIALE WL0193-001                                                
045000                                                                          
045100       MOVE 'BOLLA-PARZIALE'   TO HDR-IDOUTTYPE                           
045200       MOVE SPACE               TO HDR-IDOUTREC                           
045300       MOVE REQU-IDDC           TO HDR-IDOUTREC(1:2)                      
045400       MOVE REQU-IDUSER         TO HDR-IDOUTREC(3:8)                      
045500       MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                             
045600       PERFORM S90-PUT-DAP-HEADER                                         
045700                                                                          
045800       PERFORM S01-HAEMTA-LASTBAERARE                                     
045900                                                                          
046000       PERFORM IMS-GHNP-WL4494                                            
046100       PERFORM UNTIL SEGMENT-SAKNAS                                       
046200                                                                          
046300         IF BOLLA-4494-IDZON NOT = SPAR-IDZON                             
046400           IF NY-KUND                                                     
046500             MOVE '23        '      TO B23-IDAFPRCD                       
046600             MOVE WS-SUM-KUND-KOLLI TO B23-KVKOLLI-SUM-KUND               
046700             MOVE WS-SUM-KUND-VIKT  TO B23-VKORDBTO-SUM-KUND              
046800             MOVE WS-SUM-KUND-VOLYM TO B23-VLORDBTO-SUM-KUND              
046900                                                                          
047000             PERFORM S90-PUT-DOC-KUND-SUM                                 
047100                                                                          
047200             MOVE ZERO TO WS-SUM-KUND-KOLLI                               
047300                          WS-SUM-KUND-VIKT                                
047400                          WS-SUM-KUND-VOLYM                               
047500           END-IF                                                         
047600                                                                          
047700           MOVE '11        '     TO B11-IDAFPRCD                          
047800           MOVE BOLLA-4494-IDZON TO B11-IDZON                             
047900           PERFORM S90-PUT-DOC-HEAD                                       
048000         END-IF                                                           
048100                                                                          
048200         IF BOLLA-4494-IDKUNDNR NOT = SPAR-IDKUNDNR                       
048300                                                                          
048400           IF NY-KUND                                                     
048500             IF GAMMAL-ZONA                                               
048600               MOVE '23        '      TO B23-IDAFPRCD                     
048700               MOVE WS-SUM-KUND-KOLLI TO B23-KVKOLLI-SUM-KUND             
048800               MOVE WS-SUM-KUND-VIKT  TO B23-VKORDBTO-SUM-KUND            
048900               MOVE WS-SUM-KUND-VOLYM TO B23-VLORDBTO-SUM-KUND            
049000                                                                          
049100               PERFORM S90-PUT-DOC-KUND-SUM                               
049200                                                                          
049300               MOVE ZERO TO WS-SUM-KUND-KOLLI                             
049400                            WS-SUM-KUND-VIKT                              
049500                            WS-SUM-KUND-VOLYM                             
049600             END-IF                                                       
049700           END-IF                                                         
049800                                                                          
049900           PERFORM S10-HAEMTA-KUND-UPPGIFT                                
050000           MOVE '21        '      TO B21-IDAFPRCD                         
050100           MOVE WS-ANTAL-KUND     TO B21-IDLOPNR-KUND                     
050200*                                                                         
050300           PERFORM S90-PUT-DOC-KUND-HEAD                                  
050400*                                                                         
050500           PERFORM CC-FLYTTA-TILL-LISTA                                   
050600           PERFORM S90-PUT-DOC-LINE                                       
050700           PERFORM CD-SUMMERA-TOTALER                                     
050800         ELSE                                                             
050900           PERFORM CC-FLYTTA-TILL-LISTA                                   
051000           PERFORM S90-PUT-DOC-LINE                                       
051100           PERFORM CD-SUMMERA-TOTALER                                     
051200         END-IF                                                           
051300                                                                          
051400         MOVE BOLLA-4494-IDZON     TO SPAR-IDZON                          
051500         MOVE BOLLA-4494-IDKUNDNR  TO SPAR-IDKUNDNR                       
051600                                                                          
051700         PERFORM IMS-GHNP-WL4494                                          
051800                                                                          
051900         IF BOLLA-4494-IDKUNDNR NOT = SPAR-IDKUNDNR                       
052000           MOVE JA TO KUND-SW                                             
052100         ELSE                                                             
052200           MOVE NEJ TO KUND-SW                                            
052300         END-IF                                                           
052400         IF BOLLA-4494-IDZON NOT = SPAR-IDZON                             
052500           MOVE JA TO ZONA-SW                                             
052600         ELSE                                                             
052700           MOVE NEJ TO ZONA-SW                                            
052800         END-IF                                                           
052900       END-PERFORM                                                        
053000                                                                          
053100       MOVE '23        '      TO B23-IDAFPRCD                             
053200       MOVE WS-SUM-KUND-KOLLI TO B23-KVKOLLI-SUM-KUND                     
053300       MOVE WS-SUM-KUND-VIKT  TO B23-VLORDBTO-SUM-KUND                    
053400       MOVE WS-SUM-KUND-VOLYM TO B23-VLORDBTO-SUM-KUND                    
053500                                                                          
053600       PERFORM S90-PUT-DOC-KUND-SUM                                       
053700                                                                          
053800       MOVE ZERO TO WS-SUM-KUND-KOLLI                                     
053900                    WS-SUM-KUND-VIKT                                      
054000                    WS-SUM-KUND-VOLYM                                     
054100                                                                          
054200       MOVE '30        '     TO B30-IDAFPRCD                              
054300       MOVE WS-SUM-TOT-KOLLI TO B30-KVKOLLI-SUM-TRP                       
054400       MOVE WS-SUM-TOT-VIKT  TO B30-VKORDBTO-SUM-TRP                      
054500       MOVE WS-SUM-TOT-VOLYM TO B30-VLORDBTO-SUM-TRP                      
054600       MOVE WS-ANTAL-KUND    TO B30-IDLOPNR-KUND-MAX-TRP                  
054700       PERFORM S90-PUT-DOC-TRP-SUM                                        
054800                                                                          
054900       MOVE '40        '     TO B40-IDAFPRCD                              
055000       MOVE WS-ANTAL-BOLLA   TO B40-KVORDER-SUM-TOT                       
055100                                                                          
055200       PERFORM S90-PUT-DOC-BOLLA-TOT                                      
055300                                                                          
055400       MOVE ZERO TO WS-SUM-TOT-KOLLI                                      
055500                    WS-SUM-TOT-VIKT                                       
055600                    WS-SUM-TOT-VOLYM                                      
055700       PERFORM S90-CLOSE-DAP-SEND                                         
055800     END-IF                                                               
055900     .                                                                    
056000     EJECT                                                                
056100                                                                          
056200 S01-HAEMTA-LASTBAERARE SECTION.                                          
056300     MOVE 'S01-HAEMTA-LASTBAERARE '    TO WS-PGM-POS                      
056400                                                                          
056500     PERFORM IMS-GHNP-FIRST-WL4492                                        
056600     IF SEGMENT-FINNS                                                     
056700       MOVE +1 TO INDX                                                    
056800       MOVE NEJ TO TRAFF-SW                                               
056900                                                                          
057000       PERFORM UNTIL (INDX > MAX-INDX)  OR TRAFF                          
057100         IF BOLLA-4492-IDTRPTNR (INDX) = WS-IDTRPTNR                      
057200           MOVE BOLLA-4492-BETRPFIR (INDX) TO                             
057300                B11-BETRPFIR                                              
057400           MOVE BOLLA-4492-ADTRPFIR-RAD1 (INDX) TO                        
057500                B11-ADTRPFIR-RAD1                                         
057600           MOVE BOLLA-4492-ADTRPFIR-RAD2 (INDX) TO                        
057700                B11-ADTRPFIR-RAD2                                         
057800           MOVE BOLLA-4492-KDFRAKT       (INDX) TO                        
057900                B11-KDFRAKT                                               
058000                WS-KDFRAKT                                                
058100           MOVE JA TO TRAFF-SW                                            
058200         ELSE                                                             
058300           MOVE 'Spedizioniere mancante'     TO B11-BETRPFIR              
058400         END-IF                                                           
058500                                                                          
058600         ADD +1 TO INDX                                                   
058700       END-PERFORM                                                        
058800                                                                          
058900*SECT CA- ANV. I BÅDE C-LAES OCH D-LAES                                   
059000*      IF REQU-KDSVAR = 'P'                                               
059100*        MOVE BOLLA-4492-IDTRPBOR TO WS-IDTRPBOR                          
059200*      ELSE                                                               
059300*        IF BOLLA-4494-IDTRPBOR = ZERO                                    
059400*          MOVE BOLLA-4492-IDTRPBOR TO WS-IDTRPBOR                        
059500*        END-IF                                                           
059600*      END-IF                                                             
059700     END-IF                                                               
059800     .                                                                    
059900     EJECT                                                                
060000 CC-FLYTTA-TILL-LISTA SECTION.                                            
060100     MOVE 'CC-FLYTTA-TILL-LISTA  '     TO WS-PGM-POS                      
060200                                                                          
060300     MOVE '22        '        TO B22-IDAFPRCD                             
060400     MOVE BOLLA-4494-IDORDNR7 TO B22-IDORDNR                              
060500     MOVE BOLLA-4494-TIORDREG TO B22-TIORDREG                             
060600     MOVE BOLLA-4494-KVKOLLI  TO B22-KVKOLLI                              
060700     MOVE BOLLA-4494-VKORDBTO TO B22-VKORDBTO                             
060800     MOVE BOLLA-4494-VLORDBTO TO B22-VLORDBTO                             
060900     MOVE 'H'                 TO B22-IDTRPBOT                             
061000     MOVE BOLLA-4494-IDTRPBON TO B22-IDTRPBON                             
061100                                                                          
061200     IF WS-IDTRPTNR = 300                                                 
061300       MOVE 'PORTO FRANCO ' TO B22-TEXT1                                  
061400     ELSE                                                                 
061500       MOVE SPACE           TO B22-TEXT1                                  
061600     END-IF                                                               
061700                                                                          
061800     ADD +1 TO WS-ANTAL-BOLLA                                             
061900     .                                                                    
062000     EJECT                                                                
062100                                                                          
062200 CD-SUMMERA-TOTALER SECTION.                                              
062300     MOVE 'STA CD-SUMMERA-TOTALER'     TO WS-PGM-POS                      
062400                                                                          
062500     ADD BOLLA-4494-KVKOLLI   TO WS-SUM-KUND-KOLLI                        
062600     ADD BOLLA-4494-VKORDBTO  TO WS-SUM-KUND-VIKT                         
062700     ADD BOLLA-4494-VLORDBTO  TO WS-SUM-KUND-VOLYM                        
062800                                                                          
062900     ADD BOLLA-4494-KVKOLLI   TO WS-SUM-TOT-KOLLI                         
063000     ADD BOLLA-4494-VKORDBTO  TO WS-SUM-TOT-VIKT                          
063100     ADD BOLLA-4494-VLORDBTO  TO WS-SUM-TOT-VOLYM                         
063200                                                                          
063300     .                                                                    
063400     EJECT                                                                
063500 D-LAES-SKRIV-TOTAL SECTION.                                              
063600     MOVE 'STA D-LAES-SKRIV-TOTAL'     TO WS-PGM-POS                      
063700                                                                          
063800     MOVE REQU-IDDC        TO W-IDDC                                      
063900     MOVE REQU-IDDC        TO B11-IDDC                                    
064000     MOVE +0 TO WS-ANTAL-KUND                                             
064100                WS-ANTAL-BOLLA                                            
064200                                                                          
064300     MOVE REQU-TIDATUM TO W-DALASTN-MIN                                   
064400                          W-DALASTN-MAX                                   
064500     IF REQU-TIDATUM NOT = ZERO                                           
064600       IF REQU-TIDATUM < 500000                                           
064700         MOVE 20       TO W-DALASTN-MIN (1:2)                             
064800                          W-DALASTN-MAX (1:2)                             
064900       ELSE                                                               
065000         IF REQU-TIDATUM < 999999                                         
065100           MOVE 19     TO W-DALASTN-MIN (1:2)                             
065200                          W-DALASTN-MAX (1:2)                             
065300         ELSE                                                             
065400           MOVE 99999999 TO W-DALASTN-MIN                                 
065500                            W-DALASTN-MAX                                 
065600         END-IF                                                           
065700       END-IF                                                             
065800     END-IF                                                               
065900     MOVE REQU-IDTRPTNR TO W-IDTRPTNR-MIN                                 
066000                          W-IDTRPTNR-MAX                                  
066100                          WS-IDTRPTNR                                     
066200                                                                          
066300     MOVE NEJ TO KUND-SW                                                  
066400                 ZONA-SW                                                  
066500                                                                          
066600     MOVE LOW-VALUE TO SPAR-IDZON                                         
066700     MOVE ZERO  TO SPAR-IDKUNDNR                                          
066800     MOVE REQU-IDDC TO W-IDDC                                             
066900                                                                          
067000     PERFORM IMS-GHU-WL4491                                               
067100     IF SEGMENT-FINNS                                                     
067200                                                                          
067300       PERFORM S01-HAEMTA-LASTBAERARE                                     
067400       MOVE BOLLA-4492-IDTRPBOR TO WS-IDTRPBOR                            
067500                                                                          
067600       PERFORM IMS-GHNP-WL4494-TRP                                        
067700                                                                          
067800       IF BOLLA-4494-IDTRPBOR = ZERO                                      
067900**4492-IDTRPBOR LÄSES IN I S01-HAEMTA-LASTBAERARE 2 RADER TIDGARE         
068000         MOVE JA TO VCOM-SW                                               
068100                    BOLLADOK-SW                                           
068200       ELSE                                                               
068300         MOVE BOLLA-4494-IDTRPBOR TO WS-IDTRPBOR                          
068400         MOVE NEJ TO VCOM-SW                                              
068500                     BOLLADOK-SW                                          
068600       END-IF                                                             
068700**VCOM START                                                              
068800      IF VCOM-SKRIVS                                                      
068900        PERFORM S90-OPEN-DAP-SEND-VCOM                                    
069000                                                                          
069100        MOVE 001                 TO HDR-REQU-IDMSGVER                     
069200        MOVE REQU-KDPGMACT       TO HDR-REQU-KDPGMACT                     
069300        MOVE REQU-IDUSER         TO HDR-REQU-IDUSER                       
069400                                                                          
069500        MOVE 'BOLLA-VCOM'        TO HDR-IDOUTTYPE                         
069600        MOVE SPACE               TO HDR-IDOUTREC                          
069700        MOVE REQU-IDDC           TO HDR-IDOUTREC(1:2)                     
069800        MOVE REQU-IDUSER         TO HDR-IDOUTREC(3:8)                     
069900        MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                            
070000                                                                          
070100        PERFORM S90-PUT-DAP-HEADER                                        
070200      END-IF                                                              
070300*                                                                         
070400************   FÖRST SKRIVS VCOMFIL TILL SUSA (ALLA TRPTNR)               
070500*                                                                         
070600      IF VCOM-SKRIVS                                                      
070700        PERFORM UNTIL SEGMENT-SAKNAS                                      
070800            IF BOLLA-4494-IDTRPBOR = ZERO                                 
070900              MOVE WS-IDTRPBOR             TO VCOM-IDTRPBOR               
071000            ELSE                                                          
071100              MOVE BOLLA-4494-IDTRPBOR     TO VCOM-IDTRPBOR               
071200            END-IF                                                        
071300            MOVE WS-KDFRAKT                TO VCOM-KDFRAKT                
071400            MOVE BOLLA-4494-IDZON          TO VCOM-IDZON                  
071500            MOVE BOLLA-4494-IDKUNDNR       TO VCOM-IDKUNDNR               
071600            MOVE BOLLA-4494-IDORDNR7(3:5) TO VCOM-IDORDNR5                
071700            MOVE BOLLA-4494-TIORDREG       TO VCOM-TIORDREG               
071800            MOVE BOLLA-4494-KVKOLLI        TO VCOM-KVKOLLI                
071900            MOVE BOLLA-4494-VKORDBTO       TO VCOM-VKORDBTO               
072000            MOVE BOLLA-4494-VLORDBTO       TO VCOM-VLORDBTO               
072100            MOVE BOLLA-4494-IDTRPBON       TO WS-IDTRPBON                 
072200            MOVE WS-BOLLANR                TO VCOM-IDTRPBO                
072300            MOVE VCOM-DATUM                TO VCOM-TIREGDAT               
072400                                                                          
072500            PERFORM S90-PUT-DOC-VCOM                                      
072600          PERFORM IMS-GHNP-WL4494-TRP                                     
072700        END-PERFORM                                                       
072800                                                                          
072900        PERFORM S90-CLOSE-DAP-SEND                                        
073000      END-IF                                                              
073100*                                                                         
073200      PERFORM S90-OPEN-DAP-SEND-WEB                                       
073300*                                                                         
073400      MOVE 001             TO HDR-REQU-IDMSGVER                           
073500      MOVE REQU-KDPGMACT     TO HDR-REQU-KDPGMACT                         
073600      MOVE REQU-IDUSER       TO HDR-REQU-IDUSER                           
073700                                                                          
073800*BOLLA-FINALE WL0193-004                                                  
073900      MOVE 'BOLLA-FINALE'        TO HDR-IDOUTTYPE                         
074000      MOVE SPACE                 TO HDR-IDOUTREC                          
074100      MOVE REQU-IDDC             TO HDR-IDOUTREC(1:2)                     
074200      MOVE REQU-IDUSER           TO HDR-IDOUTREC(3:8)                     
074300      MOVE WS-YYMMDDHHMM         TO HDR-IDLIST                            
074400      PERFORM S90-PUT-DAP-HEADER                                          
074500                                                                          
074600      MOVE LOW-VALUE TO SPAR-IDZON                                        
074700      MOVE NEJ TO KUND-SW                                                 
074800                  ZONA-SW                                                 
074900      MOVE +0     TO SPAR-IDKUNDNR                                        
075000                    WS-ANTAL-BOLLA                                        
075100                    WS-ANTAL-KUND                                         
075200                    WS-SUM-KUND-KOLLI                                     
075300                    WS-SUM-KUND-VIKT                                      
075400                    WS-SUM-KUND-VOLYM                                     
075500                    WS-ANTAL-ORDER                                        
075600                                                                          
075700       PERFORM IMS-GHNP-WL4494-TRP-FIRST                                  
075800       MOVE '23        '            TO B23-IDAFPRCD                       
075900                                                                          
076000       PERFORM UNTIL SEGMENT-SAKNAS                                       
076100                                                                          
076200         IF BOLLA-4494-IDZON NOT = SPAR-IDZON                             
076300           IF NY-KUND                                                     
076400                                                                          
076500             MOVE WS-SUM-KUND-KOLLI TO B23-KVKOLLI-SUM-KUND               
076600             MOVE WS-SUM-KUND-VIKT  TO B23-VKORDBTO-SUM-KUND              
076700             MOVE WS-SUM-KUND-VOLYM TO B23-VLORDBTO-SUM-KUND              
076800                                                                          
076900             PERFORM S90-PUT-DOC-KUND-SUM                                 
077000                                                                          
077100             MOVE ZERO              TO WS-SUM-KUND-KOLLI                  
077200                                       WS-SUM-KUND-VIKT                   
077300                                       WS-SUM-KUND-VOLYM                  
077400                                       WS-ANTAL-ORDER                     
077500           END-IF                                                         
077600           MOVE '11        '     TO B11-IDAFPRCD                          
077700           MOVE BOLLA-4494-IDZON TO B11-IDZON                             
077800           MOVE WS-IDTRPBOR      TO B11-IDTRPBOR                          
077900                                                                          
078000           PERFORM S90-PUT-DOC-HEAD                                       
078100         END-IF                                                           
078200                                                                          
078300         IF BOLLA-4494-IDKUNDNR NOT = SPAR-IDKUNDNR                       
078400                                                                          
078500           IF NY-KUND                                                     
078600             IF GAMMAL-ZONA                                               
078700               MOVE '23        '      TO B23-IDAFPRCD                     
078800               MOVE WS-SUM-KUND-KOLLI TO B23-KVKOLLI-SUM-KUND             
078900               MOVE WS-SUM-KUND-VIKT  TO B23-VKORDBTO-SUM-KUND            
079000               MOVE WS-SUM-KUND-VOLYM TO B23-VLORDBTO-SUM-KUND            
079100                                                                          
079200               PERFORM S90-PUT-DOC-KUND-SUM                               
079300                                                                          
079400               MOVE ZERO            TO WS-SUM-KUND-KOLLI                  
079500                                         WS-SUM-KUND-VIKT                 
079600                                         WS-SUM-KUND-VOLYM                
079700                                         WS-ANTAL-ORDER                   
079800             END-IF                                                       
079900           END-IF                                                         
080000                                                                          
080100           PERFORM S10-HAEMTA-KUND-UPPGIFT                                
080200           MOVE '21        '      TO B21-IDAFPRCD                         
080300           MOVE WS-ANTAL-KUND     TO B21-IDLOPNR-KUND                     
080400                                                                          
080500           PERFORM S90-PUT-DOC-KUND-HEAD                                  
080600                                                                          
080700           PERFORM DE-FLYTTA-TILL-LISTA3                                  
080800           PERFORM S90-PUT-DOC-LINE                                       
080900           PERFORM DF-SUMMERA-TOTALER                                     
081000           IF BOLLA-4494-IDTRPBOR = ZERO                                  
081100             MOVE WS-IDTRPBOR TO BOLLA-4494-IDTRPBOR                      
081200             PERFORM IMS-REPLACE-WL4494                                   
081300           END-IF                                                         
081400         ELSE                                                             
081500           PERFORM DE-FLYTTA-TILL-LISTA3                                  
081600           PERFORM S90-PUT-DOC-LINE                                       
081700           PERFORM DF-SUMMERA-TOTALER                                     
081800           IF BOLLA-4494-IDTRPBOR = ZERO                                  
081900             MOVE WS-IDTRPBOR TO BOLLA-4494-IDTRPBOR                      
082000             PERFORM IMS-REPLACE-WL4494                                   
082100           END-IF                                                         
082200         END-IF                                                           
082300                                                                          
082400         MOVE BOLLA-4494-IDKUNDNR TO SPAR-IDKUNDNR                        
082500         MOVE BOLLA-4494-IDZON    TO SPAR-IDZON                           
082600                                                                          
082700         PERFORM IMS-GHNP-WL4494-TRP                                      
082800                                                                          
082900         IF BOLLA-4494-IDKUNDNR NOT = SPAR-IDKUNDNR                       
083000           MOVE JA TO KUND-SW                                             
083100         ELSE                                                             
083200           MOVE NEJ TO KUND-SW                                            
083300         END-IF                                                           
083400                                                                          
083500         IF BOLLA-4494-IDZON NOT = SPAR-IDZON                             
083600           MOVE JA TO ZONA-SW                                             
083700         ELSE                                                             
083800           MOVE NEJ TO ZONA-SW                                            
083900         END-IF                                                           
084000                                                                          
084100       END-PERFORM                                                        
084200*                                                                         
084300       MOVE '23        '      TO B23-IDAFPRCD                             
084400       MOVE WS-SUM-KUND-KOLLI TO B23-KVKOLLI-SUM-KUND                     
084500       MOVE WS-SUM-KUND-VIKT  TO B23-VKORDBTO-SUM-KUND                    
084600       MOVE WS-SUM-KUND-VOLYM TO B23-VLORDBTO-SUM-KUND                    
084700                                                                          
084800       PERFORM S90-PUT-DOC-KUND-SUM                                       
084900                                                                          
085000       MOVE ZERO              TO WS-SUM-KUND-KOLLI                        
085100                                 WS-SUM-KUND-VIKT                         
085200                                 WS-SUM-KUND-VOLYM                        
085300                                 WS-ANTAL-ORDER                           
085400*                                                                         
085500       MOVE '30        '      TO B30-IDAFPRCD                             
085600       MOVE WS-SUM-TOT-KOLLI  TO B30-KVKOLLI-SUM-TRP                      
085700       MOVE WS-SUM-TOT-VIKT   TO B30-VLORDBTO-SUM-TRP                     
085800       MOVE WS-SUM-TOT-VOLYM  TO B30-VLORDBTO-SUM-TRP                     
085900       MOVE WS-ANTAL-KUND     TO B30-IDLOPNR-KUND-MAX-TRP                 
086000                                                                          
086100       PERFORM S90-PUT-DOC-TRP-SUM                                        
086200                                                                          
086300       MOVE '40        '      TO B40-IDAFPRCD                             
086400       MOVE WS-ANTAL-BOLLA    TO B40-KVORDER-SUM-TOT                      
086500                                                                          
086600       PERFORM S90-PUT-DOC-BOLLA-TOT                                      
086700                                                                          
086800       PERFORM S90-CLOSE-DAP-SEND                                         
086900       MOVE ZERO        TO WS-SUM-TOT-KOLLI                               
087000                           WS-SUM-TOT-VIKT                                
087100                           WS-SUM-TOT-VOLYM                               
087200     END-IF                                                               
087300     MOVE 'END D-LAES-SKRIV-TOTAL'     TO WS-PGM-POS                      
087400     .                                                                    
087500     EJECT                                                                
087600 DE-FLYTTA-TILL-LISTA3 SECTION.                                           
087700     MOVE 'STA DE-FLYTTA-TILL    '     TO WS-PGM-POS                      
087800                                                                          
087900     MOVE '22        '        TO B22-IDAFPRCD                             
088000     MOVE BOLLA-4494-IDORDNR7 TO B22-IDORDNR                              
088100     MOVE BOLLA-4494-TIORDREG TO B22-TIORDREG                             
088200     MOVE BOLLA-4494-KVKOLLI  TO B22-KVKOLLI                              
088300     MOVE BOLLA-4494-VKORDBTO TO B22-VKORDBTO                             
088400     MOVE BOLLA-4494-VLORDBTO TO B22-VLORDBTO                             
088500     MOVE 'H'                 TO B22-IDTRPBOT                             
088600     MOVE BOLLA-4494-IDTRPBON TO B22-IDTRPBON                             
088700                                                                          
088800     IF WS-IDTRPTNR = 300                                                 
088900       MOVE 'PORTO FRANCO ' TO B22-TEXT1                                  
089000     ELSE                                                                 
089100       MOVE SPACE           TO B22-TEXT1                                  
089200     END-IF                                                               
089300                                                                          
089400     ADD +1 TO WS-ANTAL-BOLLA                                             
089500     .                                                                    
089600     EJECT                                                                
089700                                                                          
089800 DF-SUMMERA-TOTALER SECTION.                                              
089900     MOVE 'STA DF-SUMMERA-TOT    '     TO WS-PGM-POS                      
090000                                                                          
090100     ADD BOLLA-4494-KVKOLLI   TO WS-SUM-KUND-KOLLI                        
090200     ADD BOLLA-4494-VKORDBTO  TO WS-SUM-KUND-VIKT                         
090300     ADD BOLLA-4494-VLORDBTO  TO WS-SUM-KUND-VOLYM                        
090400                                                                          
090500     ADD BOLLA-4494-KVKOLLI   TO WS-SUM-TOT-KOLLI                         
090600     ADD BOLLA-4494-VKORDBTO  TO WS-SUM-TOT-VIKT                          
090700     ADD BOLLA-4494-VLORDBTO  TO WS-SUM-TOT-VOLYM                         
090800                                                                          
090900     ADD +1 TO WS-ANTAL-ORDER                                             
091000     .                                                                    
091100     EJECT                                                                
091200 E-LAES-SKRIV-NY-VCOM SECTION.                                            
091300     MOVE 'STA E-LAES-SKRIV-NY-VCOM'   TO WS-PGM-POS                      
092000                                                                          
093503     MOVE REQU-TIDATUM TO W-DALASTN-MIN                                   
093603                          W-DALASTN-MAX                                   
093703     IF REQU-TIDATUM NOT = ZERO                                           
093803       IF REQU-TIDATUM < 500000                                           
093903         MOVE 20       TO W-DALASTN-MIN (1:2)                             
094003                          W-DALASTN-MAX (1:2)                             
094103       ELSE                                                               
094203         IF REQU-TIDATUM < 999999                                         
094303           MOVE 19     TO W-DALASTN-MIN (1:2)                             
094403                          W-DALASTN-MAX (1:2)                             
094503         ELSE                                                             
094603           MOVE 99999999 TO W-DALASTN-MIN                                 
094703                            W-DALASTN-MAX                                 
094803         END-IF                                                           
094903       END-IF                                                             
095003     END-IF                                                               
095103                                                                          
095203     MOVE REQU-IDTRPTNR TO W-IDTRPTNR-MIN                                 
095303                           W-IDTRPTNR-MAX                                 
095403                           WS-IDTRPTNR                                    
095503                                                                          
095603     MOVE REQU-IDLBBET TO W-IDLBBET-SOEK                                  
095703                                                                          
095800     MOVE REQU-IDDC TO W-IDDC                                             
095900                                                                          
096000     PERFORM IMS-GHU-WL4491                                               
096100     IF SEGMENT-FINNS                                                     
096200                                                                          
096300       PERFORM S01-HAEMTA-LASTBAERARE                                     
096402                                                                          
096503       PERFORM IMS-GHNP-WL4494                                            
096602                                                                          
096702       MOVE BOLLA-4494-IDTRPBOR TO WS-IDTRPBOR                            
096802                                                                          
096902       PERFORM S90-OPEN-DAP-SEND-VCOM                                     
097002                                                                          
097102       MOVE 001                 TO HDR-REQU-IDMSGVER                      
097202       MOVE REQU-KDPGMACT       TO HDR-REQU-KDPGMACT                      
097302       MOVE REQU-IDUSER         TO HDR-REQU-IDUSER                        
097402                                                                          
097506       MOVE 'VCOM-IT-ARCESE'    TO HDR-IDOUTTYPE                          
097602       MOVE SPACE               TO HDR-IDOUTREC                           
097702       MOVE REQU-IDDC           TO HDR-IDOUTREC(1:2)                      
097902       MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                             
098002                                                                          
098102       PERFORM S90-PUT-DAP-HEADER                                         
098202*                                                                         
098302       PERFORM UNTIL SEGMENT-SAKNAS                                       
098402         IF BOLLA-4494-IDTRPBOR = ZERO                                    
098502           MOVE WS-IDTRPBOR                TO VCOM2-IDTRPBOR              
098602         ELSE                                                             
098702           MOVE BOLLA-4494-IDTRPBOR        TO VCOM2-IDTRPBOR              
098802         END-IF                                                           
098902         MOVE WS-KDFRAKT                   TO VCOM2-KDFRAKT               
099002         MOVE BOLLA-4494-IDZON             TO VCOM2-IDZON                 
099102         MOVE BOLLA-4494-IDDISTR           TO VCOM2-IDDISTR               
099202         MOVE BOLLA-4494-IDKUNDNR          TO VCOM2-IDKUNDNR              
099302         MOVE BOLLA-4494-IDORDNR7          TO VCOM2-IDORDNR7              
099402         MOVE BOLLA-4494-TIORDREG          TO VCOM2-TIORDREG              
099502         MOVE BOLLA-4494-IDKOLLI           TO VCOM2-IDKOLLI               
099602         MOVE BOLLA-4494-VKORDBTO          TO VCOM2-VKORDBTO              
099702         MOVE BOLLA-4494-VLORDBTO          TO VCOM2-VLORDBTO              
099802         MOVE BOLLA-4494-IDTRPBON          TO WS-IDTRPBON                 
099902         MOVE WS-BOLLANR                   TO VCOM2-IDTRPBO               
100002         MOVE VCOM-DATUM                   TO VCOM2-TIREGDAT              
100102                                                                          
100202         IF BOLLA-4494-KVKOLLI > 1                                        
100302           MOVE BOLLA-4494-IDPRODNR        TO W-IDPRODNR-X                
100402           PERFORM IMS-GU-WDE601                                          
100502           IF SEGMENT-FINNS                                               
100602             PERFORM IMS-GNP-WDE611                                       
100702             PERFORM UNTIL SEGMENT-SAKNAS                                 
100703               IF KOLLI-IDTRPTNR = REQU-IDTRPTNR AND                      
100704                  KOLLI-IDLBBET  = REQU-IDLBBET                           
100802                 MOVE KOLLI-IDKOLLI        TO VCOM2-IDKOLLI               
100902                 MOVE KOLLI-VKORDBTO-KOLLI TO VCOM2-VKORDBTO              
101002                 MOVE KOLLI-VLORDBTO-KOLLI TO VCOM2-VLORDBTO              
101102                                                                          
101202                 PERFORM S90-PUT-DOC-NY-VCOM                              
101203               END-IF                                                     
101204               PERFORM IMS-GNP-WDE611                                     
101302             END-PERFORM                                                  
101402           ELSE                                                           
101502             PERFORM S90-PUT-DOC-NY-VCOM                                  
101602           END-IF                                                         
101702         ELSE                                                             
101802           PERFORM S90-PUT-DOC-NY-VCOM                                    
101902         END-IF                                                           
102002                                                                          
102103         PERFORM IMS-GHNP-WL4494                                          
102202       END-PERFORM                                                        
102302                                                                          
103000       PERFORM S90-CLOSE-DAP-SEND                                         
110000     END-IF                                                               
117500     .                                                                    
117600     EJECT                                                                
117820 S02-RETURN-RESPONSE SECTION.                                             
117905     MOVE 'STA S02-RETURN-RESPONSE'     TO  WS-PGM-POS                    
118005                                                                          
118105     MOVE SPACE TO RESP-AREA                                              
118205     MOVE '001' TO RESP-IDMSGVER                                          
118305     MOVE 'VCO' TO RESP-IDMSG-INFO                                        
118306                                                                          
118307     PERFORM IMS-INSERT-MSG                                               
118505                                                                          
119705     .                                                                    
119805     EJECT                                                                
119905 S10-HAEMTA-KUND-UPPGIFT SECTION.                                         
120005     MOVE 'STA S10-HAEMTA-KUND-UPP'     TO WS-PGM-POS                     
120105                                                                          
120205     MOVE BOLLA-4494-IDDISTR  TO W-IDDISTR-WDB2                           
120305     MOVE BOLLA-4494-IDKUNDNR TO W-IDKUNDNR-WDB2                          
120405                                 B21-IDKUNDNR                             
120505                                                                          
120605     PERFORM IMS-GU-WDB201                                                
120705     IF SEGMENT-FINNS                                                     
120805       MOVE GMT-BEGMT-RAD1    TO B21-BEGMT-RAD1                           
120905       MOVE GMT-BEGMT-RAD2    TO B21-BEGMT-RAD2                           
121005                                                                          
121105       MOVE GMT-ADGMT-GATA    TO B21-ADGMT-GATA                           
121205       MOVE GMT-ADGMT-PADR    TO B21-ADGMT-PADR                           
121305                                                                          
121405       MOVE GMT-IDKUNDNR      TO B21-IDKUNDNR                             
121505                                                                          
121605     ELSE                                                                 
121705       MOVE 'CUSTOMER MISSING' TO B21-BEGMT-RAD1                          
121805     END-IF                                                               
121905                                                                          
122005     ADD +1 TO WS-ANTAL-KUND                                              
122105     MOVE 'END S10-HAEMTA-KUND-UPP'     TO WS-PGM-POS                     
122205     .                                                                    
122305     EJECT                                                                
122405 S90-OPEN-DAP-SEND-VCOM SECTION.                                          
122505     MOVE 'S90-OPEN-DAP-SEND-VCOM' TO WS-S90-OPCL                         
122605                                                                          
122705     MOVE 'OPEN'                     TO SEND-KDFUNC                       
122805*    -- VCOM SEND SHOULD HAVE HIGHER PRIO TO FINISH FIRST                 
122905*    -- DISTRWEB = WZ0420U HAS HIGHER PRIO THAN WZ0420X                   
122906*    -- DO NOT CHANGE THIS!                                               
123005     MOVE 'CARPARTS.DAP.DISTRWEB'    TO SEND-ADDISPABS                    
123105     CALL WZ01SEND USING SEND-CONTROL-AREA                                
123205                         SEND-OPEN-AREA                                   
123305     IF SEND-KDRC > ZERO                                                  
123405       MOVE SEND-KDRC                TO KDRC-DISPLAY                      
123505       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
123605       DELIMITED BY SIZE INTO FELTEXT                                     
123705       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
123805     END-IF                                                               
123905     .                                                                    
124005     SKIP3                                                                
124105 S90-OPEN-DAP-SEND-WEB SECTION.                                           
124205     MOVE 'S90-OPEN-DAP-SEND-WEB' TO WS-S90-OPCL                          
124305                                                                          
124405     MOVE 'OPEN'                     TO SEND-KDFUNC                       
124505*    -- WEB RESPONSE SHOULD HAVE LOWER PRIO TO FINISH LAST                
124605*    -- DISTRDOC = WZ0420X HAS LOWER PRIO THAN WZ0420U                    
124606*    -- DO NOT CHANGE THIS!                                               
124705     MOVE 'CARPARTS.DAP.DISTRDOC'    TO SEND-ADDISPABS                    
124805     CALL WZ01SEND USING SEND-CONTROL-AREA                                
124905                         SEND-OPEN-AREA                                   
125005     IF SEND-KDRC > ZERO                                                  
125105       MOVE SEND-KDRC                TO KDRC-DISPLAY                      
125205       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
125305       DELIMITED BY SIZE INTO FELTEXT                                     
125405       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
125505     END-IF                                                               
125605     .                                                                    
125705     SKIP3                                                                
125805 S90-CLOSE-DAP-SEND SECTION.                                              
125905     MOVE 'S90-CLOSE-DAP-SEND' TO WS-S90-POS                              
126005                                                                          
126105     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
126205     CALL WZ01SEND USING SEND-CONTROL-AREA                                
126305                                                                          
126405     IF SEND-KDRC > 0                                                     
126505       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
126605       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
126705       DELIMITED BY SIZE INTO FELTEXT                                     
126805       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
126905     END-IF                                                               
127005     .                                                                    
127105     SKIP3                                                                
127205 S90-PUT-DAP-HEADER SECTION.                                              
127305     MOVE 'S90-PUT-DAP-HE' TO WS-S90-POS                                  
127405                                                                          
127505     MOVE 'PUT'                           TO SEND-KDFUNC                  
127605     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
127705     CALL WZ01SEND USING SEND-CONTROL-AREA                                
127805                         SEND-KVDLEN                                      
127905                         HDR-AREA                                         
128005     IF SEND-KDRC > ZERO                                                  
128105       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
128205       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
128305       DELIMITED BY SIZE INTO FELTEXT                                     
128405       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
128505     END-IF                                                               
128605     .                                                                    
128705     SKIP3                                                                
128805 S90-PUT-DOC-VCOM SECTION.                                                
128905     MOVE 'S90-PUT-DOC-VCOM ' TO WS-S90-POS                               
129005                                                                          
129105     MOVE 'PUT'                           TO SEND-KDFUNC                  
129205     MOVE LENGTH OF LISTRAD-VCOM          TO SEND-KVDLEN                  
129305     CALL WZ01SEND USING SEND-CONTROL-AREA                                
129405                         SEND-KVDLEN                                      
129505                         LISTRAD-VCOM                                     
129605     IF SEND-KDRC > ZERO                                                  
129705       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
129805       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
129905       DELIMITED BY SIZE INTO FELTEXT                                     
130005       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
130105     END-IF                                                               
130205     .                                                                    
130305     SKIP3                                                                
130405 S90-PUT-DOC-NY-VCOM SECTION.                                             
130505     MOVE 'S90-PUT-DOC-NY-VCOM ' TO WS-S90-POS                            
130605                                                                          
130705     MOVE 'PUT'                           TO SEND-KDFUNC                  
130805     MOVE LENGTH OF LISTRAD-VCOM2         TO SEND-KVDLEN                  
130905     CALL WZ01SEND USING SEND-CONTROL-AREA                                
131005                         SEND-KVDLEN                                      
131105                         LISTRAD-VCOM2                                    
131205     IF SEND-KDRC > ZERO                                                  
131305       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
131405       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
131505       DELIMITED BY SIZE INTO FELTEXT                                     
131605       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
131705     END-IF                                                               
131805     .                                                                    
131905     SKIP3                                                                
132005 S90-PUT-DOC-HEAD SECTION.                                                
132105     MOVE 'STA S90-PUT-DOC-HEAD  '   TO WS-S90-POS                        
132205                                                                          
132305     MOVE 'PUT'                           TO SEND-KDFUNC                  
132405     MOVE LENGTH OF DOC-HEAD-AREA         TO SEND-KVDLEN                  
132505     CALL WZ01SEND USING SEND-CONTROL-AREA                                
132605                         SEND-KVDLEN                                      
132705                         DOC-HEAD-AREA                                    
132805     IF SEND-KDRC > ZERO                                                  
132905       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
133005       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
133105       DELIMITED BY SIZE INTO FELTEXT                                     
133205       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
133305     END-IF                                                               
133405     .                                                                    
133505 S90-PUT-DOC-KUND-HEAD SECTION.                                           
133605     MOVE 'STA S90-PUT-DOC-KUND-HEAD  '   TO WS-S90-POS                   
133705                                                                          
133805     MOVE 'PUT'                           TO SEND-KDFUNC                  
133905     MOVE LENGTH OF DOC-KUND-HEAD-AREA    TO SEND-KVDLEN                  
134005     CALL WZ01SEND USING SEND-CONTROL-AREA                                
134105                         SEND-KVDLEN                                      
134205                         DOC-KUND-HEAD-AREA                               
134305     IF SEND-KDRC > ZERO                                                  
134405       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
134505       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
134605       DELIMITED BY SIZE INTO FELTEXT                                     
134705       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
134805     END-IF                                                               
134905     .                                                                    
135005                                                                          
135105 S90-PUT-DOC-LINE SECTION.                                                
135205     MOVE 'STA S90-PUT-DOC-LINE  '   TO WS-S90-POS                        
135305                                                                          
135405     MOVE 'PUT'                           TO SEND-KDFUNC                  
135505     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
135605     CALL WZ01SEND USING SEND-CONTROL-AREA                                
135705                         SEND-KVDLEN                                      
135805                         DOC-LINE-AREA                                    
135905     IF SEND-KDRC > ZERO                                                  
136005       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
136105       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
136205       DELIMITED BY SIZE INTO FELTEXT                                     
136305       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
136405     END-IF                                                               
136505     .                                                                    
136605                                                                          
136705 S90-PUT-DOC-KUND-SUM   SECTION.                                          
136805     MOVE 'STA S90-PUT-DOC-KUND-SUM'   TO WS-S90-POS                      
136905                                                                          
137005     MOVE 'PUT'                           TO SEND-KDFUNC                  
137105     MOVE LENGTH OF DOC-KUND-SUM-AREA     TO SEND-KVDLEN                  
137205     CALL WZ01SEND USING SEND-CONTROL-AREA                                
137305                         SEND-KVDLEN                                      
137405                         DOC-KUND-SUM-AREA                                
137505     IF SEND-KDRC > ZERO                                                  
137605       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
137705       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
137805       DELIMITED BY SIZE INTO FELTEXT                                     
137905       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
138005     END-IF                                                               
138105     .                                                                    
138205                                                                          
138305 S90-PUT-DOC-TRP-SUM   SECTION.                                           
138405     MOVE 'STA S90-PUT-DOC-TRP-SUM'   TO WS-S90-POS                       
138505                                                                          
138605     MOVE 'PUT'                           TO SEND-KDFUNC                  
138705     MOVE LENGTH OF DOC-TRP-SUM-AREA     TO SEND-KVDLEN                   
138805     CALL WZ01SEND USING SEND-CONTROL-AREA                                
138905                         SEND-KVDLEN                                      
139005                         DOC-TRP-SUM-AREA                                 
139105     IF SEND-KDRC > ZERO                                                  
139205       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
139305       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
139405       DELIMITED BY SIZE INTO FELTEXT                                     
139505       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
139605     END-IF                                                               
139705     .                                                                    
139805                                                                          
139905 S90-PUT-DOC-BOLLA-TOT SECTION.                                           
140005     MOVE 'STA S90-PUT-DOC-BOLLA-TOT'   TO WS-S90-POS                     
140105                                                                          
140205     MOVE 'PUT'                         TO SEND-KDFUNC                    
140305     MOVE LENGTH OF DOC-BOLLA-TOT-AREA  TO SEND-KVDLEN                    
140405     CALL WZ01SEND USING SEND-CONTROL-AREA                                
140505                         SEND-KVDLEN                                      
140605                         DOC-BOLLA-TOT-AREA                               
140705     IF SEND-KDRC > ZERO                                                  
140805       MOVE SEND-KDRC                   TO KDRC-DISPLAY                   
140905       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
141005       DELIMITED BY SIZE INTO FELTEXT                                     
141105       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
141205     END-IF                                                               
141305     .                                                                    
141405     EJECT                                                                
141505                                                                          
141605* --- IMS SEKTIONER ---                                                   
141705*                                                                         
141805*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
141905*                 III     III MM MMMMM MM SSSS   SSSS                     
142005*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
142105*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
142205*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
142305*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
142405*                 III     III MM MMMMM MM SSSS   SSSS                     
142505*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
142605*                                                                         
142705     SKIP3                                                                
142805 IMS-GET-MSG SECTION.                                                     
142905     MOVE 'IMS-GET-MSG    '   TO WS-IMS-SEC                               
143005                                                                          
143105     MOVE '  QC' TO GODK-STATUSKODER                                      
143205     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
143305     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
143405     PERFORM IMS-STATUSKONTROLL                                           
143505     .                                                                    
143605     SKIP3                                                                
143606 IMS-INSERT-MSG SECTION.                                                  
143607                                                                          
143621     MOVE LENGTH OF MSG-IO-AREA2 TO MSG-KVLL2                             
143622     MOVE LOW-VALUE              TO MSG-KDZZ2                             
143630     MOVE SPACE TO GODK-STATUSKODER                                       
143640     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA2                         
143650     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
143660     PERFORM IMS-STATUSKONTROLL                                           
143670     .                                                                    
143680     EJECT                                                                
143705 IMS-GU-WDB201 SECTION.                                                   
143805     MOVE 'IMS-GU-WDB201    '   TO WS-IMS-SEC                             
143905                                                                          
144005     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
144105          DELIMITED BY SIZE INTO SSA1                                     
144205     MOVE '  ' TO GODK-STATUSKODER                                        
144305     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA2 SSA1                     
144405     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
144505     PERFORM IMS-STATUSKONTROLL                                           
144605     .                                                                    
144705     EJECT                                                                
144805 IMS-GHU-WL4491 SECTION.                                                  
144905     MOVE 'IMS-GHU-WL4491   '   TO WS-IMS-SEC                             
145005                                                                          
145105     STRING 'WL449101(WDGXKEY  =' W-WDGXKEY-X ')'                         
145205          DELIMITED BY SIZE INTO SSA1                                     
145305     MOVE '  ' TO GODK-STATUSKODER                                        
145405     CALL CBLTDLI USING GHU BOLLA-PCB DLI-IO-AREA SSA1                    
145505     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
145605     PERFORM IMS-STATUSKONTROLL                                           
145705     .                                                                    
145805     SKIP2                                                                
145905 IMS-GHNP-WL4494 SECTION.                                                 
146005     MOVE 'IMS-GHNP-WL4494  '   TO WS-IMS-SEC                             
146105                                                                          
146205     STRING 'WL449112(KY4494  >=' W-KY4494-MIN-X                          
146305                    '&KY4494  <=' W-KY4494-MAX-X                          
146405                    '&IDLBBET  =' W-IDLBBET-X ')'                         
146505          DELIMITED BY SIZE INTO SSA1                                     
146605     MOVE '  GE' TO GODK-STATUSKODER                                      
146705     CALL CBLTDLI USING GHNP BOLLA-PCB DLI-IO-AREA1 SSA1                  
146805     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
146905     PERFORM IMS-STATUSKONTROLL                                           
147005     .                                                                    
147105     SKIP2                                                                
147205 IMS-GHNP-WL4494-TRP SECTION.                                             
147305     MOVE 'IMS-GHNP-WL4494-TRP' TO WS-IMS-SEC                             
147405                                                                          
147505     STRING 'WL449112(KY4494  >=' W-KY4494-MIN-X                          
147605                    '&KY4494  <=' W-KY4494-MAX-X ')'                      
147705          DELIMITED BY SIZE INTO SSA1                                     
147805     MOVE '  GE' TO GODK-STATUSKODER                                      
147905     CALL CBLTDLI USING GHNP BOLLA-PCB DLI-IO-AREA1 SSA1                  
148005     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
148105     PERFORM IMS-STATUSKONTROLL                                           
148205     .                                                                    
148305     SKIP2                                                                
148405 IMS-GHNP-WL4494-TRP-FIRST SECTION.                                       
148505     MOVE 'IMS-GHNP-WL4494-TRP-FIRST' TO WS-IMS-SEC                       
148605                                                                          
148705     STRING 'WL449112*F(KY4494  >=' W-KY4494-MIN-X                        
148805                      '&KY4494  <=' W-KY4494-MAX-X ')'                    
148905          DELIMITED BY SIZE INTO SSA1                                     
149005     MOVE '  GE' TO GODK-STATUSKODER                                      
149105     CALL CBLTDLI USING GHNP BOLLA-PCB DLI-IO-AREA1 SSA1                  
149205     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
149305     PERFORM IMS-STATUSKONTROLL                                           
149405     .                                                                    
149505     SKIP2                                                                
149605 IMS-GHNP-FIRST-WL4492 SECTION.                                           
149705     MOVE 'IMS-GHNP-FIRST-WL4492' TO WS-IMS-SEC                           
149805                                                                          
149905     MOVE 'WL449111*F' TO SSA1                                            
150005     MOVE '  ' TO GODK-STATUSKODER                                        
150105     CALL CBLTDLI USING GHNP BOLLA-PCB DLI-IO-AREA SSA1                   
150205     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
150305     PERFORM IMS-STATUSKONTROLL                                           
150405     .                                                                    
150505     EJECT                                                                
150605 IMS-REPLACE-WL4492 SECTION.                                              
150705     MOVE 'IMS-REPLACE-WL4492 ' TO WS-IMS-SEC                             
150805     SKIP2                                                                
150905     MOVE '  ' TO GODK-STATUSKODER                                        
151005     CALL CBLTDLI USING REPL BOLLA-PCB DLI-IO-AREA                        
151105     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
151205     PERFORM IMS-STATUSKONTROLL                                           
151305     .                                                                    
151405     SKIP2                                                                
151505 IMS-REPLACE-WL4494 SECTION.                                              
151605     MOVE 'IMS-REPLACE-WL4494 ' TO WS-IMS-SEC                             
151705     SKIP2                                                                
151805     MOVE '  ' TO GODK-STATUSKODER                                        
151905     CALL CBLTDLI USING REPL BOLLA-PCB DLI-IO-AREA1                       
152005     MOVE BOLLA-STATUS-CODE TO STATUS-WS                                  
152105     PERFORM IMS-STATUSKONTROLL                                           
152205     .                                                                    
152305     EJECT                                                                
152405                                                                          
152505 IMS-GU-WDE601 SECTION.                                                   
152605                                                                          
152705     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
152805            DELIMITED BY SIZE INTO SSA1                                   
152905     MOVE '  ' TO GODK-STATUSKODER                                        
153005     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
153105     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
153205     PERFORM IMS-STATUSKONTROLL                                           
153305     .                                                                    
153405     EJECT                                                                
153406 IMS-GNP-WDE611 SECTION.                                                  
153407                                                                          
153408     MOVE 'WDE611 ' TO SSA1                                               
153409     MOVE '  GE' TO GODK-STATUSKODER                                      
153410     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
153420     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
153430     PERFORM IMS-STATUSKONTROLL                                           
153440     .                                                                    
153450     EJECT                                                                
153505 IMS-STATUSKONTROLL SECTION.                                              
153605                                                                          
153705     SET STATUS-IX TO 1                                                   
153805     SEARCH GODK-STATUS                                                   
153905       AT END                                                             
154005         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
154105         DELIMITED BY SIZE INTO FELTEXT                                   
154205         CALL FELLOG                                                      
154305       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
154405         CONTINUE                                                         
155005     END-SEARCH                                                           
160000     .                                                                    
