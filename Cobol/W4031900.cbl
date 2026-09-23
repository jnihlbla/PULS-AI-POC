000100 ID DIVISION.                                                             
000202 PROGRAM-ID.     W4031900.                                                
000302 AUTHOR.         LOVISH KUMAR.                                            
000415 DATE-WRITTEN.   10/11/2022.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.PULS.CHECKNOTREPORTED'                         
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PROGRAMMET KONTROLLERAR ATT DE TILL PACKAREN UTDELADE            
001100*        ORDERRADERNA ÄR FÄRDIGBEHANDLADE.                                
001200*        OM GODKÄND STARTAS BAKGRUNDSTRANS W40397 SOM UPPDATERAR          
001300*        ORDER- OCH ARTIKELREGISTREN. DESSUTOM SKAPAS EN TRANS-           
001400*        AKTION FÖR AVVIKELSERAD FÖR TRANSAKTIONER TILL ÖVRIGA            
001500*        SYSTEM OCH LÅSNINGSREG SLÅS AV.                                  
001600*        AUTOMATFAKTURATRANS SKAPAS I VISSA FALL.                         
001900*                                                                         
002016*  W4031900 PROGRAM IS A REPLICA OF WL012300 PROGRAM                      
002116*  AND CUSTOMIZED FOR WMS REQUIREMENTS.                                   
002200*                                                                         
002300*    INDATA.                                                              
002405*        TRANSACTION: W40319U                                             
002505*        REQUEST:     W40319I1                                            
002600*                                                                         
002700*    OUTDATA.                                                             
002805*        RESPONSE:    W40319O1                                            
002900*                                                                         
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004515 77  IDPGM                       PIC X(08) VALUE 'W4031900'.              
004600                                                                          
004700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004800 77  FILLER                      PIC X(08) VALUE 'FELTEXT:'.              
004900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005000 77  KDRC-DISPLAY                PIC Z(5).                                
005220 77  FILLER                      PIC X(08)   VALUE 'CURRSECT'.            
005320 77  WS-CURRENT-SECTION          PIC X(25)   VALUE SPACE.                 
005420 77  FILLER                      PIC X(08)   VALUE 'IMS-SECT'.            
005520 77  WS-CURRENT-IMS-SECTION      PIC X(25)   VALUE SPACE.                 
005615 77  JA                          PIC X     VALUE 'J'.                     
005715 77  YES                         PIC X     VALUE 'Y'.                     
005815 77  NEJ                         PIC X     VALUE 'N'.                     
005915 77  RAETT                       PIC X     VALUE 'R'.                     
006015 77  FEL                         PIC X     VALUE 'F'.                     
006016 77  NEXT-ORDPART                PIC X     VALUE 'N'.                     
006315 77  LINE-IX                     PIC S9(9) VALUE +0   COMP SYNC.          
006316 77  MSG-IX                      PIC S9(9) VALUE +0   COMP SYNC.          
006715 77  WS-KDMFSFOR                 PIC 9(1)  VALUE ZERO.                    
006815 77  WS-IDANSTNR                 PIC X(5)  VALUE SPACE.                   
006915 77  WS-IDDISTR                  PIC X(4)  VALUE SPACE.                   
007015 77  WS-IDDISTR-NUM              PIC 9(4)  VALUE ZERO.                    
007115 77  WS-IDKUNDNR                 PIC X(6)  VALUE SPACE.                   
007215 77  WS-IDKUNDNR-NUM             PIC 9(6)  VALUE ZERO.                    
007315 77  WS-IDKOLLI                  PIC 9(5)  VALUE ZERO.                    
007415 77  WS-IDPRODNR                 PIC 9(7)  VALUE ZERO.                    
007615 77  WS-IDPLKLST                 PIC 9(3)  VALUE ZERO.                    
007616 77  WS-IDPURAD                  PIC 9(4)  VALUE ZERO.                    
008215 77  WS-FLNOLLJ                  PIC X(1)  VALUE SPACE.                   
008300 77  FILLER                      PIC X(08) VALUE 'AAAAAAAA'.              
008415 77  WS-KVORAPP                  PIC 9(6)  VALUE ZERO.                    
008615 77  WS-ANT-ODEL-KVAR-ATT-BEHANDLA PIC 9(3) VALUE ZERO.                   
008815 77  WS-IDELMT-ERROR             PIC X(16).                               
008915 77  WS-IDMSG-ERROR              PIC X(03).                               
009015 77  WS-IDMSG-INFO               PIC X(03).                               
009100                                                                          
009800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009900 01  GENERAL-SUBPROGRAMS.                                                 
010000     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
010100     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
010200     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
010303     03  WZ01AUTH                PIC X(8)   VALUE 'WZ01AUTH'.             
010304     03  WMSGCONV                PIC X(8)   VALUE 'WMSGCONV'.             
010404     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
010503     SKIP3                                                                
010603*    --- PARAMETERS TO ABEND                                              
010703                                                                          
010803 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010903 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011003 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011103     EJECT                                                                
011203*                                                                         
011303 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011415     SKIP3                                                                
011503*01  -COPY WZ01SUB                                                        
011603     EJECT                                                                
011703 01  FILLER                      PIC X(16)   VALUE 'WZ01AUTH'.            
011803*01  -COPY WZ01AUTH                                                       
011903     EJECT                                                                
011904 01  FILLER                      PIC X(16)   VALUE 'WMSGCONV'.            
011905*01  -COPY WMSGCONV                                                       
011906     EJECT                                                                
012003 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012103     SKIP3                                                                
012203 01  REQU-AREA.                                                           
012303*    03  -COPY WZ01REQ2                                                   
012405*    03  -COPY W40319I1                                                   
012503     EJECT                                                                
012603 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012703     SKIP3                                                                
012803 01  RESP-AREA.                                                           
012903*    03  -COPY WZ01RES2                                                   
013005*    03  -COPY W40319O1                                                   
013103     EJECT                                                                
013203 77    MAX-ANTAL-RADER           PIC S9(3)  VALUE +500 COMP-3.            
013403 77    WS-TRAEFF-PACKARE         PIC X(01).                               
013503   88  TRAEFF-PACKARE                       VALUE 'J'.                    
014003 77    WS-KOLLIFEL               PIC X(01).                               
014103   88  KOLLIFEL                             VALUE 'J'.                    
014203     EJECT                                                                
014303 77    WS-INDATA-TEST            PIC X(01).                               
014403   88  WS-INDATA-FEL                        VALUE 'F'.                    
014503   88  WS-INDATA-RATT                       VALUE 'R'.                    
014603     SKIP2                                                                
014703 77    WS-BEHANDLING-TEST        PIC X(01).                               
014803   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
014903   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
015003     SKIP2                                                                
015303*                                                                         
016703 01    WS-IDRADNR-X                                 PIC X(4).             
016803 01    WS-IDRADNR-NUM   REDEFINES WS-IDRADNR-X      PIC 9(4).             
016903     SKIP2                                                                
017003 01    WS-ADLAGOMR-N             PIC 9(3).                                
017103 01    WS-ADLAGOMR-X REDEFINES WS-ADLAGOMR-N.                             
017203   03  FILLER                    PIC X.                                   
017303   03  WS-ADLAGOMR               PIC X(2).                                
017403     SKIP2                                                                
017503 01    WS-IDKUNDRF.                                                       
017603   03  WS-IDORDNR                PIC X(5).                                
017703   03  FILLER                    PIC X(5)   VALUE SPACE.                  
018103     SKIP2                                                                
018104 01    WS-JFR-IDANSTNR.                                                   
018105   03  FILLER                    PIC X(3).                                
018106   03  WS-JFR-IDANSTNR-5         PIC X(5).                                
018208 77    FL-ORDDEL                 PIC X(01).                               
018308   88  ORDDEL-FINNS                         VALUE 'J'.                    
018408   88  ORDDEL-SAKNAS                        VALUE 'N'.                    
018503     SKIP2                                                                
018603 77    FL-NYCKLAR                PIC X(01).                               
018703   88  FL-NYA-NYCKLAR                       VALUE 'J'.                    
018803   88  FL-GAMLA-NYCKLAR                     VALUE 'N'.                    
018903                                                                          
019003 01   ARB-ADRESS.                                                         
019103    05 ARB-ADFLGEO               PIC X(3)   VALUE SPACE.                  
019203    05 FILLER                    PIC X      VALUE SPACE.                  
019303    05 ARB-ADFLOMR               PIC 9(3)   VALUE ZERO.                   
019403    05 FILLER                    PIC X      VALUE SPACE.                  
019503    05 ARB-ADRUTNIV              PIC 9(3)   VALUE ZERO.                   
019603     SKIP2                                                                
019703     EJECT                                                                
019803 01    NYCKLAR-TILL-DLI.                                                  
020603                                                                          
020703   03    W-WDE401-KUNDORDER-X.                                            
020803     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
020903     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
021003     05    W-401-IDKUNDRF.                                                
021103       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
021203       07  FILLER                PIC X(05)   VALUE SPACE.                 
021303     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
021403     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
021503                                                                          
021603   03    W-WDE4B-KEYSEQ-MIN-X.                                            
021703     05    W-411-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
021803     05    W-411-IDPURAD-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
021903                                                                          
022003   03    W-WDE4B-KEYSEQ-MAX-X.                                            
022103     05    W-411-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
022203     05    W-411-IDPURAD-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
022303                                                                          
022420   03    W-WDE4B-KEYSEQ-X.                                                
022520     05    W-411-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
022620     05    W-411-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
022720                                                                          
022730     03  W-WDQ3HSEQ-X.                                                    
022731         05  W-IDDC-Q3H1         PIC  X(2)   VALUE SPACE.                 
022740         05  W-IDPRCPLK-Q3H1     PIC  X(4)   VALUE SPACE.                 
022750         05  W-IDLOTNRP-Q3H1     PIC S9(03)  VALUE ZERO COMP-3.           
024020     EJECT                                                                
024120     SKIP3                                                                
024220 01    MEDDELANDE.                                                        
024320     SKIP2                                                                
024420   03    FEL-X1                  PIC  X(03)  VALUE '022'.                 
024520   03    NEW-KEYS-AND-INPUT      PIC  X(03)  VALUE '182'.                 
024620   03    FEL-X3                  PIC  X(03)  VALUE '025'.                 
024720   03    FEL-X4                  PIC  X(03)  VALUE '183'.                 
024820   03    FEL-X5                  PIC  X(03)  VALUE '184'.                 
024920   03    SYSTEM-ERROR            PIC  X(03)  VALUE '099'.                 
025020   03    FEL-X7                  PIC  X(03)  VALUE '173'.                 
025120   03    FEL-X8                  PIC  X(03)  VALUE '174'.                 
025220   03    FEL-X9                  PIC  X(03)  VALUE '175'.                 
025320   03    FEL-XA                  PIC  X(03)  VALUE '176'.                 
025420   03    FEL-XB                  PIC  X(03)  VALUE '177'.                 
025520   03    FEL-XC                  PIC  X(03)  VALUE '152'.                 
025530   03    FEL-XD                  PIC  X(03)  VALUE '213'.                 
025620   03    UNPACKED-CASES-EXIST    PIC  X(03)  VALUE '178'.                 
025720   03    FEL-XE                  PIC  X(03)  VALUE '179'.                 
025820   03    FEL-XF                  PIC  X(03)  VALUE '180'.                 
025920   03    LAST-ORDER-PART         PIC  X(03)  VALUE '181'.                 
026020   03    TOO-MANY-LINES          PIC  X(03)  VALUE '028'.                 
026120   03    FEL-XI                  PIC  X(03)  VALUE '170'.                 
026220   03    FEL-XJ                  PIC  X(03)  VALUE '171'.                 
026320   03    NEXT-ORDERPART-INDICATOR                                         
026420                                 PIC  X(03)  VALUE '172'.                 
026520   03    FEL-XL                  PIC  X(03)  VALUE '205'.                 
026620   03    ERR-UNAUTHORIZED        PIC  X(03)  VALUE '00A'.                 
026720     EJECT                                                                
026820 01    FILLER                    PIC X(16)   VALUE 'TRANS-AREA '.         
026920 01    4397-TRANSAREA.                                                    
027020   03    4397-IDPRODNR           PIC 9(7)    VALUE ZERO.                  
027120   03    4397-IDANSTNR           PIC 9(5)    VALUE ZERO.                  
027220   03    4397-IDPLKLST           PIC 9(3)    VALUE ZERO.                  
027320   03    4397-IDPURAD            PIC 9(5)    VALUE ZERO.                  
027420   03    FILLER                  PIC X(80)   VALUE SPACE.                 
027520     EJECT                                                                
027620 01    FILLER                    PIC X(16)   VALUE 'ALT-IO-AREA'.         
027720 01    ALT-IO-AREA.                                                       
027820   03    ALT-LL                  PIC S9(4)   COMP  SYNC.                  
027920   03    ALT-Z1                  PIC X(1).                                
028020   03    ALT-Z2                  PIC X(1).                                
028120   03    ALT-TRANSKOD            PIC X(8)    VALUE SPACE.                 
028220   03    ALT-IDTRANS             PIC X(4)    VALUE SPACE.                 
028320   03    ALT-KDMFSFOR            PIC X(1)    VALUE SPACE.                 
028420   03    ALT-AREA                PIC X(105)  VALUE SPACE.                 
028520     SKIP2                                                                
028620******************************************************************        
028720*                                                                         
028820*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028920*                                                                         
029020 01    IMS-WS.                                                            
029120   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
029220     SKIP3                                                                
029320*                        **** STATUS-KOD FRÅN IMS                         
030020   03    STATUS-WS               PIC XX.                                  
030120     88    SEGMENT-FINNS                     VALUE '  '.                  
030220     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
030320     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
030330     88    SEGMENT-END                       VALUE 'GB'.                  
030420     SKIP3                                                                
030520   03    GODK-STATUSKODER.                                                
030620     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
030720     SKIP3                                                                
030820 01    SSA1                      PIC X(64).                               
030920 01    SSA2                      PIC X(64).                               
031103     EJECT                                                                
031203*                            IMS FUNKTIONSKODER                           
031303*01    -COPY W0003                                                        
031403     EJECT                                                                
031503*                            DLI INPUT-OUTPUT AREA                        
031620 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-WDE4'.         
031720     SKIP3                                                                
031820 01  DLI-IO-WDE4.                                                         
031920*    03  -COPY WDE411                                                     
032120*    03  -COPY WDE401                                                     
032220     EJECT                                                                
032320 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-WDQ3'.         
032420     SKIP3                                                                
032520 01  DLI-IO-WDQ3.                                                         
032620*    03  -COPY WDQ301                                                     
032720     EJECT                                                                
034220                                                                          
034320 LINKAGE SECTION.                                                         
034420*01  MSG-PCB                     PIC X.                                   
034520*01    -COPY W0009     -PRE MSG-                                          
034620     EJECT                                                                
034720*01    -COPY W0009     -PRE ALT-                                          
034820     EJECT                                                                
035220*01    -COPY W0008     -PRE WDE43-                                        
035320     05  FILLER                  PIC X.                                   
035420     EJECT                                                                
035520*01    -COPY W0008     -PRE WDQ3H-                                        
035620     05  FILLER                  PIC X.                                   
035720                                                                          
035820     EJECT                                                                
035920 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB WDE43-PCB WDQ3H-PCB.           
036020                                                                          
036120 MAIN SECTION.                                                            
036220     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDE43-PCB WDQ3H-PCB.           
036420                                                                          
036520     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
036620     IF SUB-KDRC = 0                                                      
036720       PERFORM A-INIT                                                     
036820       PERFORM B-GENERELL-KONTROLL                                        
036920         IF WS-INDATA-RATT                                                
037020            IF REQU-KDPGMACT = 'E'                                        
037030               PERFORM G-CHECK-RAD                                        
037040               IF WS-INDATA-RATT                                          
037120                 PERFORM D-KONTROLL-OK                                    
037130               END-IF                                                     
037220            ELSE                                                          
037320               PERFORM F-KONTROLL-OK-BLANK                                
037420            END-IF                                                        
037430         END-IF                                                           
038703       PERFORM S30-MSG-CONV                                               
038803       PERFORM S02-RETURN-RESPONSE                                        
038903     END-IF                                                               
039003     MOVE ZERO TO RETURN-CODE                                             
039103     GOBACK                                                               
039203     .                                                                    
039303     EJECT                                                                
039403 A-INIT SECTION.                                                          
039503                                                                          
039804     MOVE 001                             TO RESP-IDRESVER                
039903     MOVE SPACE                           TO RESP-IDMSG-ERROR             
040003                                             RESP-IDMSG-INFO              
040103                                             RESP-IDELMT-ERROR            
040518     MOVE ZERO                            TO RESP-KVRADER                 
040603     MOVE RAETT                           TO WS-INDATA-TEST               
040903                                                                          
041021     IF REQU-KDPGMACT = 'E' OR 'S'                                        
041121       CONTINUE                                                           
041221     ELSE                                                                 
041321       MOVE FEL                 TO WS-INDATA-TEST                         
041421       MOVE SYSTEM-ERROR        TO RESP-IDMSG-ERROR                       
041521       MOVE 'KDPGMACT'          TO RESP-IDELMT-ERROR                      
041721     END-IF                                                               
041821                                                                          
041921     IF WS-INDATA-RATT                                                    
042021       MOVE 001                  TO AUTH-KDCALL                           
042121       CALL WZ01AUTH          USING AUTH-WZ01AUTH                         
042221                                      REQU-WZ01REQ2                       
042321       IF AUTH-KDRC > 0                                                   
042421         MOVE FEL                TO WS-INDATA-TEST                        
042521         MOVE ERR-UNAUTHORIZED   TO RESP-IDMSG-ERROR                      
042721       END-IF                                                             
042821     END-IF                                                               
042903     .                                                                    
043003     EJECT                                                                
047000                                                                          
048900 B-GENERELL-KONTROLL  SECTION.                                            
049004                                                                          
049121     IF WS-INDATA-RATT                                                    
049122*IDDC                                                                     
049221       IF REQU-IDDC IS > SPACE                                            
049321         MOVE REQU-IDDC         TO RESP-IDDC                              
049421       ELSE                                                               
049521         MOVE FEL               TO WS-INDATA-TEST                         
049621         MOVE FEL-X1            TO RESP-IDMSG-ERROR                       
049721         MOVE 'IDDC'            TO RESP-IDELMT-ERROR                      
049921       END-IF                                                             
050221*IDANSTNR                                                                 
050421       IF (REQU-IDANSTNR NOT NUMERIC)                                     
050521       OR (REQU-IDANSTNR = ZERO)                                          
050621           MOVE FEL             TO WS-INDATA-TEST                         
050721           MOVE FEL-X1          TO RESP-IDMSG-ERROR                       
050821           MOVE 'IDANSTNR'      TO RESP-IDELMT-ERROR                      
050921       ELSE                                                               
051021           MOVE REQU-IDANSTNR   TO WS-IDANSTNR                            
051121       END-IF                                                             
051321       MOVE WS-IDANSTNR     TO   RESP-IDANSTNR                            
051421       INSPECT RESP-IDANSTNR REPLACING LEADING ZERO BY SPACE              
051621*IDPRC                                                                    
053321       IF (REQU-IDPRCPLK(1:3) NOT NUMERIC)                                
053521           MOVE FEL             TO WS-INDATA-TEST                         
053621           MOVE FEL-X1          TO RESP-IDMSG-ERROR                       
053721           MOVE 'IDPRC'         TO RESP-IDELMT-ERROR                      
053821       ELSE                                                               
053921           MOVE REQU-IDPRCPLK     TO RESP-IDPRCPLK                        
054021       END-IF                                                             
054022*IDLOTNR                                                                  
054521       IF (REQU-IDLOTNR-PLK  NOT NUMERIC)                                 
054621       OR (REQU-IDLOTNR-PLK  = ZERO)                                      
054721           MOVE FEL             TO WS-INDATA-TEST                         
054821           MOVE FEL-X1          TO RESP-IDMSG-ERROR                       
054921           MOVE 'IDUSER'        TO RESP-IDELMT-ERROR                      
055021       ELSE                                                               
055121           MOVE REQU-IDLOTNR-PLK  TO RESP-IDLOTNR-PLK                     
055221       END-IF                                                             
055222     END-IF                                                               
055223                                                                          
572321     IF WS-INDATA-RATT                                                    
572921       PERFORM BB-KOLL-REQU-FLSVAR                                        
573021     END-IF                                                               
573100     .                                                                    
574000     EJECT                                                                
575104 BB-KOLL-REQU-FLSVAR SECTION.                                             
575218                                                                          
575219     IF REQU-KVRADER NOT NUMERIC                                          
575220        MOVE ZERO TO REQU-KVRADER                                         
575230     END-IF                                                               
575240                                                                          
575312     MOVE +1 TO LINE-IX                                                   
575412     PERFORM UNTIL LINE-IX > REQU-KVRADER                                 
575413       EVALUATE TRUE                                                      
575414       WHEN REQU-FLSVAR (LINE-IX) = ' '                                   
575415         MOVE 'N'     TO REQU-FLSVAR (LINE-IX)                            
576021       WHEN REQU-FLSVAR (LINE-IX) = 'J' OR 'Y' OR 'N'                     
576112          CONTINUE                                                        
576212       WHEN OTHER                                                         
576312          MOVE FEL                TO WS-INDATA-TEST                       
576412          MOVE FEL-XJ             TO RESP-IDMSG-ERROR                     
576512       END-EVALUATE                                                       
576513       ADD +1 TO LINE-IX                                                  
576612     END-PERFORM                                                          
576700     .                                                                    
576800     EJECT                                                                
576900 G-CHECK-RAD  SECTION.                                                    
576910                                                                          
577000     MOVE +1       TO LINE-IX                                             
577100                                                                          
577200     PERFORM UNTIL LINE-IX > REQU-KVRADER                                 
577210               OR WS-INDATA-FEL                                           
577300       MOVE REQU-IDPRODNR(LINE-IX) TO WS-IDPRODNR                         
577400       MOVE REQU-IDPLKLST(LINE-IX) TO WS-IDPLKLST                         
577500       MOVE REQU-IDRADNR (LINE-IX) TO WS-IDPURAD                          
577600       MOVE WS-IDPRODNR           TO W-411-IDPRODNR                       
577700       MOVE WS-IDPURAD            TO W-411-IDPURAD                        
578000       PERFORM IMS-GHU-KUNDORDER-SEK-INV-KVAL                             
578100                                                                          
578200       IF SEGMENT-FINNS                                                   
578800         IF REQU-FLSVAR (LINE-IX) = 'Y'                                   
579000            IF ORAD-KDRADSTA > 3                                          
579100              MOVE FEL    TO WS-INDATA-TEST                               
579200              MOVE FEL-XD TO RESP-IDMSG-ERROR                             
579500            END-IF                                                        
580100         END-IF                                                           
580200       ELSE                                                               
580210         MOVE FEL         TO   WS-INDATA-TEST                             
580220         MOVE FEL-X3      TO   RESP-IDMSG-ERROR                           
580230         MOVE 'IDORDNR'   TO   RESP-IDELMT-ERROR                          
580300       END-IF                                                             
580400       ADD +1 TO LINE-IX                                                  
580500     END-PERFORM                                                          
580600     .                                                                    
599112 D-KONTROLL-OK  SECTION.                                                  
599218                                                                          
599219     MOVE REQU-IDDC               TO W-IDDC-Q3H1                          
599220     MOVE REQU-IDPRCPLK           TO W-IDPRCPLK-Q3H1                      
599221     MOVE REQU-IDLOTNR-PLK        TO W-IDLOTNRP-Q3H1                      
599240     MOVE +1                      TO LINE-IX                              
599250                                                                          
599260     PERFORM IMS-GU-WDQ301                                                
599280     IF SEGMENT-FINNS                                                     
599282      IF REQU-KVRADER NOT > ZERO                                          
599284       PERFORM UNTIL SEGMENT-SAKNAS                                       
599285                  OR SEGMENT-END                                          
599286          MOVE ODEL-IDPRODNR TO WS-IDPRODNR                               
599287          MOVE ODEL-IDPLKLST TO WS-IDPLKLST                               
599288          MOVE ZERO          TO WS-IDPURAD                                
599289          PERFORM DA-SKAPA-BAKGRUNDTRANS                                  
599290          PERFORM IMS-GN-WDQ301                                           
599292       END-PERFORM                                                        
599293      ELSE                                                                
599295        PERFORM UNTIL SEGMENT-SAKNAS                                      
599300                   OR SEGMENT-END                                         
599310                   OR LINE-IX > MAX-ANTAL-RADER                           
599312         MOVE 'N' TO NEXT-ORDPART                                         
599313                                                                          
599317         IF ODEL-IDPRODNR = REQU-IDPRODNR(LINE-IX)                        
599318         AND ODEL-IDPLKLST = REQU-IDPLKLST(LINE-IX)                       
599321           PERFORM UNTIL LINE-IX > REQU-KVRADER                           
599322                     OR NEXT-ORDPART = 'Y'                                
599327             MOVE REQU-IDPRODNR(LINE-IX) TO WS-IDPRODNR                   
599328             MOVE REQU-IDPLKLST(LINE-IX) TO WS-IDPLKLST                   
599329             MOVE REQU-IDRADNR (LINE-IX) TO WS-IDPURAD                    
599330             MOVE WS-IDPRODNR     TO W-411-IDPRODNR                       
599331             MOVE WS-IDPURAD      TO W-411-IDPURAD                        
599332             PERFORM IMS-GHU-KUNDORDER-SEK-INV-KVAL                       
599333                                                                          
599334             IF SEGMENT-FINNS                                             
599335               IF REQU-FLSVAR (LINE-IX) = 'Y'                             
599337                  PERFORM D-KONTROLL-OK-JA                                
599338               ELSE                                                       
599339                IF REQU-FLSVAR (LINE-IX) = 'N'                            
599341                   PERFORM D-KONTROLL-OK-NEJ                              
599342                END-IF                                                    
599343               END-IF                                                     
599344             END-IF                                                       
599345                                                                          
599347             IF LINE-IX < REQU-KVRADER                                    
599348               ADD +1 TO LINE-IX                                          
599349             ELSE                                                         
599350               MOVE 'Y'  TO  NEXT-ORDPART                                 
599351             END-IF                                                       
599352                                                                          
599353             IF ODEL-IDPRODNR NOT = REQU-IDPRODNR(LINE-IX)                
599355               MOVE 'Y'  TO  NEXT-ORDPART                                 
599356             ELSE                                                         
599358              IF ODEL-IDPLKLST NOT = REQU-IDPLKLST(LINE-IX)               
599360                 MOVE 'Y' TO NEXT-ORDPART                                 
599361              END-IF                                                      
599362             END-IF                                                       
599363           END-PERFORM                                                    
599364         ELSE                                                             
599366           MOVE ODEL-IDPRODNR TO WS-IDPRODNR                              
599367           MOVE ODEL-IDPLKLST TO WS-IDPLKLST                              
599368           MOVE ZERO          TO WS-IDPURAD                               
599369           PERFORM DA-SKAPA-BAKGRUNDTRANS                                 
599370         END-IF                                                           
599371         PERFORM IMS-GN-WDQ301                                            
599372       END-PERFORM                                                        
599373      END-IF                                                              
599374     ELSE                                                                 
599375       MOVE FEL               TO WS-INDATA-TEST                           
599376       MOVE FEL-X3            TO RESP-IDMSG-ERROR                         
599377       MOVE 'ORDPART'         TO RESP-IDELMT-ERROR                        
599380     END-IF                                                               
603408     .                                                                    
603508     EJECT                                                                
603612 D-KONTROLL-OK-JA   SECTION.                                              
603708                                                                          
605012     IF KORD-FLPAFEL = NEJ                                                
605212        MOVE +1      TO KORD-KDPAKOLL                                     
605312        PERFORM IMS-REPL-KUNDORDER                                        
605412        PERFORM DA-SKAPA-BAKGRUNDTRANS                                    
605413        MOVE FEL-X8  TO RESP-IDMSG-ERR-LINE(LINE-IX)                      
605512     ELSE                                                                 
605612        MOVE FEL-X5  TO RESP-IDMSG-ERROR                                  
605812        MOVE FEL-X7  TO RESP-IDMSG-INFO                                   
606012        MOVE +0      TO KORD-KDPAKOLL                                     
606112        MOVE NEJ     TO KORD-FLPAFEL                                      
606212        PERFORM IMS-REPL-KUNDORDER                                        
606312     END-IF                                                               
606408     .                                                                    
606508     EJECT                                                                
607512 DA-SKAPA-BAKGRUNDTRANS  SECTION.                                         
607608                                                                          
607708     MOVE 'W4T397X '              TO  ALT-TRANSKOD                        
607808                                      ALT-LTERM-NAME                      
607908     MOVE WS-KDMFSFOR             TO  ALT-KDMFSFOR                        
608008     MOVE '4319'                  TO  ALT-IDTRANS                         
608108     MOVE +117                    TO  ALT-LL                              
608208     MOVE WS-IDPRODNR             TO  4397-IDPRODNR                       
608308     MOVE WS-IDANSTNR             TO  4397-IDANSTNR                       
608418     MOVE WS-IDPLKLST             TO  4397-IDPLKLST                       
608519     MOVE WS-IDPURAD              TO  4397-IDPURAD                        
608608     MOVE 4397-TRANSAREA          TO  ALT-AREA                            
608708     PERFORM IMS-PURG-ALTMSG                                              
608908     .                                                                    
609008     EJECT                                                                
609812 D-KONTROLL-OK-NEJ SECTION.                                               
609908                                                                          
610808     MOVE +0           TO KORD-KDPAKOLL                                   
610908     MOVE NEJ          TO KORD-FLPAFEL                                    
611008     PERFORM IMS-REPL-KUNDORDER                                           
611009     MOVE FEL-X9    TO RESP-IDMSG-ERR-LINE(LINE-IX)                       
611308     .                                                                    
611408     EJECT                                                                
611500 F-KONTROLL-OK-BLANK SECTION.                                             
611510                                                                          
620518     MOVE REQU-IDDC               TO W-IDDC-Q3H1                          
621510     MOVE REQU-IDPRCPLK           TO W-IDPRCPLK-Q3H1                      
621610     MOVE REQU-IDLOTNR-PLK        TO W-IDLOTNRP-Q3H1                      
621811     MOVE +1                      TO LINE-IX                              
621910                                                                          
622120     PERFORM IMS-GU-WDQ301                                                
622210                                                                          
622220     IF SEGMENT-FINNS                                                     
622310      PERFORM UNTIL SEGMENT-SAKNAS                                        
622410                 OR SEGMENT-END                                           
622511                 OR LINE-IX > MAX-ANTAL-RADER                             
622710        MOVE ODEL-IDPRODNR        TO WS-IDPRODNR                          
622910        MOVE WS-IDPRODNR          TO W-411-IDPRODNR-MIN                   
623010                                     W-411-IDPRODNR-MAX                   
623210        MOVE 1                    TO W-411-IDPURAD-MIN                    
623310        MOVE 99999                TO W-411-IDPURAD-MAX                    
623410                                                                          
623510        PERFORM IMS-GHU-KUNDORDER-SEK-INV                                 
623710          IF SEGMENT-FINNS                                                
623810           IF KORD-IDDC = REQU-IDDC                                       
623910             MOVE 'J'  TO FL-ORDDEL                                       
624010             PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-END                  
624110             OR ORDDEL-SAKNAS                                             
624211             OR LINE-IX > MAX-ANTAL-RADER                                 
624510               IF ODEL-IDPRODNR = KORD-IDPRODNR                           
624520                MOVE KORD-IDUSER    TO WS-JFR-IDANSTNR                    
624610                IF ODEL-IDPLKLST = KORD-IDPLKLST                          
624620                AND WS-IDANSTNR  = WS-JFR-IDANSTNR-5                      
625711                 MOVE KORD-IDPRODNR TO RESP-IDPRODNR(LINE-IX)             
626111                 MOVE KORD-IDPLKLST TO RESP-IDPLKLST(LINE-IX)             
626211                                                                          
627310                 IF KORD-KDPAKOLL = 2                                     
627911*LK                MOVE FEL      TO WS-INDATA-TEST                        
628011                   MOVE FEL-XC   TO RESP-IDMSG-ERR-LINE(LINE-IX)          
628210                 ELSE                                                     
628410                   IF KORD-KDPAKOLL = ZERO                                
628510                      MOVE NEJ          TO KORD-FLPAFEL                   
628610                      PERFORM IMS-REPL-KUNDORDER                          
628710                   END-IF                                                 
628810                 END-IF                                                   
628910                                                                          
629010                 IF WS-INDATA-RATT                                        
630310                   IF ORAD-KDRADSTA < 4                                   
630511                     MOVE ORAD-IDPURAD  TO RESP-IDRADNR(LINE-IX)          
630611                     MOVE ORAD-ADLAGOMR TO RESP-ADLAGOMR(LINE-IX)         
630711                     MOVE ORAD-IDARTNR  TO RESP-IDARTNR (LINE-IX)         
630811                     MOVE ORAD-KVAVBART TO RESP-KVAVBART(LINE-IX)         
631011                     COMPUTE WS-KVORAPP =                                 
631111                                   ORAD-KVAVBART - ORAD-KVLEVART          
631211                     MOVE WS-KVORAPP    TO RESP-KVORAPP(LINE-IX)          
631311                                                                          
631411                     MOVE ORAD-FLNOLLJ  TO WS-FLNOLLJ                     
631511                     IF WS-FLNOLLJ = JA                                   
631611                       MOVE 'Y'         TO RESP-FLNOLLJ(LINE-IX)          
631711                     ELSE                                                 
631811                       MOVE WS-FLNOLLJ  TO RESP-FLNOLLJ(LINE-IX)          
631911                     END-IF                                               
632411                     IF RESP-FLNOLLJ (LINE-IX) = 'N'                      
632511                        MOVE FEL-XL          TO                           
632611                                  RESP-IDMSG-ERR-LINE(LINE-IX)            
632711                                                                          
632811                     END-IF                                               
632813                     MOVE LINE-IX  TO RESP-KVRADER                        
632911                     ADD +1    TO LINE-IX                                 
633011                   END-IF                                                 
633111                 END-IF                                                   
633810                                                                          
634011                 IF LINE-IX  < 500                                        
634711                    CONTINUE                                              
635411                 ELSE                                                     
635511                   MOVE TOO-MANY-LINES TO RESP-IDMSG-ERROR                
635611                 END-IF                                                   
635740                END-IF                                                    
636110               ELSE                                                       
636210                  MOVE 'N' TO FL-ORDDEL                                   
636310               END-IF                                                     
636510               PERFORM IMS-GHN-KUNDORDER-SEK-INV                          
636610             END-PERFORM                                                  
636710           ELSE                                                           
636810             MOVE FEL                   TO   WS-INDATA-TEST               
636910             MOVE FEL-X3                TO   RESP-IDMSG-ERROR             
637010             MOVE 'IDORDNR'             TO   RESP-IDELMT-ERROR            
637110           END-IF                                                         
637210          ELSE                                                            
637310             MOVE FEL                   TO   WS-INDATA-TEST               
637410             MOVE FEL-X3                TO   RESP-IDMSG-ERROR             
637510             MOVE 'IDORDNR'             TO   RESP-IDELMT-ERROR            
637610          END-IF                                                          
637720        PERFORM IMS-GN-WDQ301                                             
637810      END-PERFORM                                                         
637820     ELSE                                                                 
637821       MOVE FEL                   TO   WS-INDATA-TEST                     
637822       MOVE FEL-X3                TO   RESP-IDMSG-ERROR                   
637823       MOVE 'ORDPART'             TO   RESP-IDELMT-ERROR                  
637830     END-IF                                                               
637910                                                                          
639110     .                                                                    
639138 S30-MSG-CONV SECTION.                                                    
639139     MOVE SPACES                  TO RESP-MESSAGES (1)                    
639140                                     RESP-MESSAGES (2)                    
639150     MOVE 1                       TO MSG-IX                               
639160*    REQUEST OK                                                           
639170     MOVE 200                     TO RESP-KDSTATUS-API                    
639180     IF RESP-IDMSG-INFO > SPACE                                           
639190       MOVE SPACES                TO MSG-CONV-AREA                        
639200       MOVE RESP-IDMSG-INFO       TO MSG-CONV-IDMSG-IN                    
639201       CALL WMSGCONV           USING MSG-CONV-AREA                        
639202       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
639203       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
639204       ADD 1                      TO MSG-IX                               
639205     END-IF                                                               
639206     IF RESP-IDMSG-ERROR > SPACE                                          
639207*      BAD REQUEST                                                        
639208       MOVE 400                   TO RESP-KDSTATUS-API                    
639209       MOVE SPACES                TO MSG-CONV-AREA                        
639210       MOVE RESP-IDMSG-ERROR      TO MSG-CONV-IDMSG-IN                    
639211       MOVE RESP-IDELMT-ERROR     TO MSG-CONV-IDELMT                      
639212       CALL WMSGCONV           USING MSG-CONV-AREA                        
639213       MOVE MSG-CONV-IDMSG-OUT    TO RESP-IDMSG   (MSG-IX)                
639214       MOVE MSG-CONV-MESSAGE      TO RESP-MESSAGE (MSG-IX)                
639215     END-IF                                                               
639216     .                                                                    
639220     EJECT                                                                
659810*    --- DISPATCHER SECTIONS                                              
659910 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
660010                                                                          
660110     MOVE 'GETARG'               TO SUB-KDFUNC                            
660210     MOVE 'CARPARTS.PULS.CHECKNOTREPORTED'   TO SUB-ADDISPABS             
660310     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
660410                                                                          
660510     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
660610                                                                          
660710     IF SUB-KDRC > 0                                                      
660810       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
660910       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
661010       DELIMITED BY SIZE INTO ERROR-TEXT                                  
661110       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
661210     END-IF                                                               
661310     .                                                                    
661410     SKIP3                                                                
661510 S02-RETURN-RESPONSE SECTION.                                             
661610                                                                          
661710     MOVE 'RETURN'                   TO SUB-KDFUNC                        
661810     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
661910                                                                          
662010     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
662110                                                                          
662210     IF SUB-KDRC > 0                                                      
662310       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
662410       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
662510       DELIMITED BY SIZE INTO ERROR-TEXT                                  
662610       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
662710     END-IF                                                               
662810     .                                                                    
662910     EJECT                                                                
663010*IMS                                                                      
663110 IMS-PURG-ALTMSG SECTION.                                                 
663210                                                                          
663310     MOVE LOW-VALUE TO ALT-Z1 ALT-Z2                                      
663410     MOVE SPACE TO GODK-STATUSKODER                                       
663510     CALL CBLTDLI USING PURG ALT-PCB ALT-IO-AREA                          
663610     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
663710     PERFORM IMS-STATUSKONTROLL                                           
663810     .                                                                    
663910     SKIP3                                                                
672410 IMS-GHU-KUNDORDER-SEK-INV SECTION.                                       
672520     MOVE 'GU-KUNDORDER-SEK'     TO WS-CURRENT-IMS-SECTION                
672620                                                                          
672720     STRING 'WDE411  *D(WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X ')'              
672810                      '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'              
672910            DELIMITED BY SIZE INTO SSA1                                   
673010     MOVE 'WDE401' TO SSA2                                                
673110     MOVE '  GEGB'  TO GODK-STATUSKODER                                   
673220     CALL CBLTDLI USING GHU   WDE43-PCB DLI-IO-WDE4 SSA1 SSA2             
673310     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
673410     PERFORM IMS-STATUSKONTROLL                                           
673510     .                                                                    
673610     EJECT                                                                
673718 IMS-GHN-KUNDORDER-SEK-INV SECTION.                                       
673820     MOVE 'GN-KUNDORDER-SEK'     TO WS-CURRENT-IMS-SECTION                
673920                                                                          
674020     STRING 'WDE411  *D(WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X ')'              
674118                      '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'              
674218            DELIMITED BY SIZE INTO SSA1                                   
674318     MOVE 'WDE401' TO SSA2                                                
674418     MOVE '  GEGB'  TO GODK-STATUSKODER                                   
674520     CALL CBLTDLI USING GHN   WDE43-PCB DLI-IO-WDE4 SSA1 SSA2             
674618     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
674718     PERFORM IMS-STATUSKONTROLL                                           
674818     .                                                                    
674918     EJECT                                                                
675020 IMS-GHU-KUNDORDER-SEK-INV-KVAL SECTION.                                  
675120     MOVE 'SEK-INV-KVAL'     TO WS-CURRENT-IMS-SECTION                    
675220                                                                          
675320     STRING 'WDE411  *D(WDE4BSEQ =' W-WDE4B-KEYSEQ-X ')'                  
675520            DELIMITED BY SIZE INTO SSA1                                   
675620     MOVE 'WDE401' TO SSA2                                                
675720     MOVE '  GEGB'  TO GODK-STATUSKODER                                   
675820     CALL CBLTDLI USING GHU   WDE43-PCB DLI-IO-WDE4 SSA1 SSA2             
675920     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
676020     PERFORM IMS-STATUSKONTROLL                                           
676120     .                                                                    
676220     EJECT                                                                
676230 IMS-REPL-KUNDORDER     SECTION.                                          
676231     MOVE 'REPL-KUNDORDER'    TO WS-CURRENT-IMS-SECTION                   
676232                                                                          
676240     MOVE '  ' TO GODK-STATUSKODER                                        
676250     CALL CBLTDLI USING REPL WDE43-PCB DLI-IO-WDE4                        
676260     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
676270     PERFORM IMS-STATUSKONTROLL                                           
676280     .                                                                    
676290     SKIP2                                                                
676320 IMS-GU-WDQ301 SECTION.                                                   
676420     MOVE 'IMS-GU-WDQ301'     TO WS-CURRENT-IMS-SECTION                   
676518                                                                          
676618     STRING 'WDQ301  (WDQ3HSEQ =' W-WDQ3HSEQ-X ')'                        
676718          DELIMITED BY SIZE INTO SSA1                                     
676818     MOVE '  GE' TO GODK-STATUSKODER                                      
676920     CALL CBLTDLI USING GU WDQ3H-PCB DLI-IO-WDQ3 SSA1                     
677018     MOVE WDQ3H-STATUS-CODE TO STATUS-WS                                  
677118     PERFORM IMS-STATUSKONTROLL                                           
677218     .                                                                    
677318     EJECT                                                                
677420 IMS-GN-WDQ301 SECTION.                                                   
677520     MOVE 'IMS-GN-WDQ301'     TO WS-CURRENT-IMS-SECTION                   
677618                                                                          
677718     STRING 'WDQ301  (WDQ3HSEQ =' W-WDQ3HSEQ-X ')'                        
677818          DELIMITED BY SIZE INTO SSA1                                     
677918     MOVE '  GEGB' TO GODK-STATUSKODER                                    
678020     CALL CBLTDLI USING GN WDQ3H-PCB DLI-IO-WDQ3 SSA1                     
678118     MOVE WDQ3H-STATUS-CODE TO STATUS-WS                                  
678218     PERFORM IMS-STATUSKONTROLL                                           
678318     .                                                                    
678418     EJECT                                                                
678518 IMS-STATUSKONTROLL SECTION.                                              
678618     SET STATUS-IX TO 1                                                   
678718     SEARCH GODK-STATUS AT END CALL FELLOG                                
678818       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
679018     END-SEARCH                                                           
680000     .                                                                    
