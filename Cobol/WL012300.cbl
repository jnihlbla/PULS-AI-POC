      *COMPOPT VPOSIX=YES                                                       
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL012300.                                                
000300 AUTHOR.         SUBBARAO PARUCHURI V.                                    
000400 DATE-WRITTEN.   04/06/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAME:       'CARPARTS.LDC.CHECKNOTREPORTED'                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PROGRAMMET KONTROLLERAR ATT DE TILL PACKAREN UTDELADE            
001100*        ORDERRADERNA ÄR FÄRDIGBEHANDLADE.                                
001200*        OM GODKÄND STARTAS BAKGRUNDSTRANS W40397 SOM UPPDATERAR          
001300*        ORDER- OCH ARTIKELREGISTREN. DESSUTOM SKAPAS EN TRANS-           
001400*        AKTION FÖR AVVIKELSERAD FÖR TRANSAKTIONER TILL ÖVRIGA            
001500*        SYSTEM OCH LÅSNINGSREG SLÅS AV.                                  
001600*        AUTOMATFAKTURATRANS SKAPAS I VISSA FALL.                         
001700* CHECK BOX NEED TO BE CREATED FOR FEL-833 MESSAGE.THIS                   
001800* THIS WILL BE DONE AFTER SUMMER VACATION.                                
001900*                                                                         
002000*  WL012300 PROGRAM IS A REPLICA OF W4031800 PROGRAM                      
002100*  AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                               
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSACTION: WL0123U                                             
002500*        REQUEST:     WL0123I1                                            
002600*                                                                         
002700*    OUTDATA.                                                             
002800*        RESPONSE:    WL0123O1                                            
002900*                                                                         
003000*    E'TRACKER: 5522850 DAT. 20070917                                     
003100*                                                                         
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
004500 77  IDPGM                       PIC X(08)   VALUE 'WL012300'.            
004600                                                                          
004700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004800 77  FILLER                      PIC X(08) VALUE 'FELTEXT:'.              
004900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
005000 77  KDRC-DISPLAY                PIC Z(5).                                
005100 77  WS-PGM-POS                  PIC X(16) VALUE SPACE.                   
005200 77    JA                        PIC X       VALUE 'J'.                   
005300 77    YES                       PIC X       VALUE 'Y'.                   
005400 77    NEJ                       PIC X       VALUE 'N'.                   
005500 77    RAETT                     PIC X       VALUE 'R'.                   
005600 77    FEL                       PIC X       VALUE 'F'.                   
005700 77    SOEK-VIA-PRODNR           PIC X       VALUE 'N'.                   
005800 77    WS-WDE401-FEL             PIC X.                                   
005900 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
006000 77    INX                       PIC S9(9)   VALUE +0   COMP SYNC.        
006100 77    RAD-INX                   PIC S9(9)   VALUE +0   COMP SYNC.        
006200 77    BILD-RAD                  PIC S9(9)   VALUE +0   COMP SYNC.        
006300 77    WS-KDMFSFOR               PIC 9(1)   VALUE ZERO.                   
006400 77    WS-IDANSTNR               PIC X(5)   VALUE SPACE.                  
006500 77    WS-IDDISTR                PIC X(4)   VALUE SPACE.                  
006600 77    WS-IDDISTR-NUM            PIC 9(4)   VALUE ZERO.                   
006700 77    WS-IDKUNDNR               PIC X(6)   VALUE SPACE.                  
006800 77    WS-IDKUNDNR-NUM           PIC 9(6)   VALUE ZERO.                   
006900 77    WS-IDKOLLI                PIC 9(5)   VALUE ZERO.                   
007000 77    WS-IDPRODNR               PIC 9(7)   VALUE ZERO.                   
007100 77    WS-JFR-IDPRODNR           PIC X(7)   VALUE SPACE.                  
007200 77    WS-IDPLKLST               PIC 9(3)   VALUE ZERO.                   
007300 77    WS-IDPLKLST-SPAR          PIC 9(3)   VALUE ZERO.                   
007400 77    WS-IDPLKLST-NAESTA        PIC 9(3)   VALUE ZERO.                   
007500 77    WS-IDPLKLST-SPAR-AR-HOGST   PIC X.                                 
007600 77    WS-IDPLKLST-NAESTA-AR-HOGST PIC X.                                 
007700 77    WS-IDPLKLST-FOM           PIC 9(3)   VALUE ZERO.                   
007800 77    WS-JFR-IDPLKLST           PIC 9(3)   VALUE ZERO.                   
007900 77    WS-IDRADNR-SPAR           PIC 9(4)   VALUE ZERO.                   
008000 77    WS-KVORDRAD-LEVPL-SPAR    PIC S9(5) COMP-3 VALUE ZERO.             
008100 77    WS-IDRADNR-RED            PIC Z(4).                                
008200 77    WS-FLNOLLJ                PIC X(1)   VALUE SPACE.                  
008300 77  FILLER                      PIC X(08) VALUE 'AAAAAAAA'.              
008400 77    WS-KVORAPP                PIC 9(6)   VALUE ZERO.                   
008500 77    WS-KVORAPP-RED            PIC Z(6).                                
008600 77    WS-ANT-ODEL-KVAR-ATT-BEHANDLA PIC 9(3)   VALUE ZERO.               
008700 77    WS-COUNT                  PIC 9(3)   VALUE ZERO.                   
008800 77    WS-IDELMT-ERROR           PIC X(16).                               
008900 77    WS-IDMSG-ERROR            PIC X(03).                               
009000 77    WS-IDMSG-INFO             PIC X(03).                               
009100                                                                          
009200                                                                          
009300                                                                          
009400     EJECT                                                                
009500*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009600 01  GENERAL-SUBPROGRAMS.                                                 
009700     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
009800     03  ABEND                   PIC X(8)   VALUE 'ABEND   '.             
009900     03  WZ01SUB                 PIC X(8)   VALUE 'WZ01SUB '.             
010000     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
           03  W488ORCN                PIC X(8)   VALUE 'W488ORCN'.             
010100     SKIP3                                                                
010200*    --- PARAMETERS TO ABEND                                              
010300                                                                          
010400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010700     EJECT                                                                
010800*                                                                         
010900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011000     SKIP3                                                                
011100*01  -COPY WZ01SUB                                                        
011200     EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'W488ORCN'.            
      *01 -COPY W488ORCN                                                        
           EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011400     SKIP3                                                                
011500 01  REQU-AREA.                                                           
011600*    03  -COPY WZ01REQU                                                   
011700*    03  -COPY WL0123I1                                                   
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012000     SKIP3                                                                
012100 01  RESP-AREA.                                                           
012200*    03  -COPY WZ01RESP                                                   
012300*    03  -COPY WL0123O1                                                   
012400     EJECT                                                                
012500 77    MAX-ANTAL-RADER           PIC S9(3)  VALUE +500 COMP-3.            
012600 77    MAX-ANT-RADER-PLUS-1      PIC S9(3)  VALUE +501 COMP-3.            
012700 77    WS-TRAEFF-PACKARE         PIC X(01).                               
012800   88  TRAEFF-PACKARE                       VALUE 'J'.                    
012900 77    WS-FLER-ORDERDELAR-FINNS  PIC X(01).                               
013000   88  FLER-ORDERDELAR-FINNS                VALUE 'J'.                    
013100 77    WS-SLINGA-KLAR            PIC X(01).                               
013200   88  SLINGA-KLAR                          VALUE 'J'.                    
013300 77    WS-KOLLIFEL               PIC X(01).                               
013400   88  KOLLIFEL                             VALUE 'J'.                    
013500     EJECT                                                                
013600 77    WS-INDATA-TEST            PIC X(01).                               
013700   88  WS-INDATA-FEL                        VALUE 'F'.                    
013800   88  WS-INDATA-RATT                       VALUE 'R'.                    
013900     SKIP2                                                                
014000 77    WS-BEHANDLING-TEST        PIC X(01).                               
014100   88  WS-BEHANDLING-FEL                    VALUE 'F'.                    
014200   88  WS-BEHANDLING-RATT                   VALUE 'R'.                    
014300     SKIP2                                                                
014400 77    WS-4397-STARTAD-SW        PIC X(01).                               
014500   88  WS-4397-STARTAD                      VALUE 'J'.                    
014600*                                                                         
014700 01  WS-IDPRTLST.                                                         
014800     03 WS-SYSTDEL               PIC X(1).                                
014900     03 WS-LISTTYP               PIC X(2).                                
015000     03 WS-DC                    PIC X(2).                                
015100     03 WS-KDPRT                 PIC X(3).                                
015200                                                                          
015300 01    TESTMED.                                                           
015400   03  TESTMED1                 PIC 9(3).                                 
015500   03  FILLER                   PIC X(2).                                 
015600   03  TESTMED2                 PIC 9(5).                                 
015700   03  FILLER                   PIC X(2).                                 
015800   03  TESTMED3                 PIC X(1).                                 
015900   03  FILLER                   PIC X(27).                                
016000 01    WS-IDRADNR-X                                 PIC X(4).             
016100 01    WS-IDRADNR-NUM   REDEFINES WS-IDRADNR-X      PIC 9(4).             
016200     SKIP2                                                                
016300 01    WS-ADLAGOMR-N             PIC 9(3).                                
016400 01    WS-ADLAGOMR-X REDEFINES WS-ADLAGOMR-N.                             
016500   03  FILLER                    PIC X.                                   
016600   03  WS-ADLAGOMR               PIC X(2).                                
016700     SKIP2                                                                
016800 01    WS-IDKUNDRF.                                                       
016900   03  WS-IDORDNR                PIC X(5).                                
017000   03  FILLER                    PIC X(5)   VALUE SPACE.                  
017100 01    WS-JFR-IDANSTNR.                                                   
017200   03  FILLER                    PIC X(3).                                
017300   03  WS-JFR-IDANSTNR-5         PIC X(5).                                
017400     SKIP2                                                                
017500 77    FL-531-SEGMENT            PIC X(01).                               
017600   88  ORAPP-RADER-FINNS                    VALUE 'J'.                    
017700   88  ORAPP-RADER-SAKNAS                   VALUE 'N'.                    
017800     SKIP2                                                                
017900 77    FL-NYCKLAR                PIC X(01).                               
018000   88  FL-NYA-NYCKLAR                       VALUE 'J'.                    
018100   88  FL-GAMLA-NYCKLAR                     VALUE 'N'.                    
018200                                                                          
018300 01   ARB-ADRESS.                                                         
018400    05 ARB-ADFLGEO               PIC X(3)   VALUE SPACE.                  
018500    05 FILLER                    PIC X      VALUE SPACE.                  
018600    05 ARB-ADFLOMR               PIC 9(3)   VALUE ZERO.                   
018700    05 FILLER                    PIC X      VALUE SPACE.                  
018800    05 ARB-ADRUTNIV              PIC 9(3)   VALUE ZERO.                   
018900     SKIP2                                                                
019000     EJECT                                                                
019100 01    NYCKLAR-TILL-DLI.                                                  
019200                                                                          
019300   03    W-WDE4A1-KUNDORDER-X.                                            
019400     05    W-4A1-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
019500     05    W-4A1-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
019600     05    W-4A1-IDKUNDRF.                                                
019700       07  W-4A1-IDORDNR         PIC  9(5)   VALUE ZERO.                  
019800       07  FILLER                PIC X(05)   VALUE SPACE.                 
019900                                                                          
020000   03    W-WDE401-KUNDORDER-X.                                            
020100     05    W-401-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.          
020200     05    W-401-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.          
020300     05    W-401-IDKUNDRF.                                                
020400       07  W-401-IDORDNR         PIC  9(5)   VALUE ZERO.                  
020500       07  FILLER                PIC X(05)   VALUE SPACE.                 
020600     05    W-401-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
020700     05    W-401-IDPLKLST        PIC S9(3)   VALUE ZERO  COMP-3.          
020800                                                                          
020900   03    W-WDE4B-KEYSEQ-MIN-X.                                            
021000     05    W-411-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO  COMP-3.          
021100     05    W-411-IDPURAD-MIN     PIC S9(5)   VALUE ZERO  COMP-3.          
021200                                                                          
021300   03    W-WDE4B-KEYSEQ-MAX-X.                                            
021400     05    W-411-IDPRODNR-MAX    PIC S9(7)   VALUE ZERO  COMP-3.          
021500     05    W-411-IDPURAD-MAX     PIC S9(5)   VALUE ZERO  COMP-3.          
021600                                                                          
021700   03    W-WDE411-IDPURAD-X.                                              
021800     05    W-411-IDPURAD         PIC S9(5)   VALUE ZERO  COMP-3.          
021900                                                                          
022000   03    W-WDE601-IDPRODNR-X.                                             
022100     05    W-601-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
022200                                                                          
022300   03    W-4301-WDGXKEY-X.                                                
022400     05    W-4301-IDHTYP         PIC X(4)    VALUE '4301'.                
022500     05    W-4301-IDPRODNR       PIC S9(7)   VALUE ZERO  COMP-3.          
022600     05    W-4301-NYCKEL-VALFRI  PIC X(22)   VALUE LOW-VALUE.             
022700                                                                          
022800   03    W-4302-WDGXKEY-X.                                                
022900     05    W-4302-IDKOLLI        PIC S9(5)   VALUE ZERO  COMP-3.          
023000     05    W-4302-IDPLKLST       PIC S9(3)   VALUE ZERO  COMP-3.          
023100                                                                          
023200     EJECT                                                                
023300     SKIP3                                                                
023400 01    MEDDELANDE.                                                        
023500     SKIP2                                                                
023600   03    FEL-X1                  PIC  X(03)  VALUE '022'.                 
023700   03    NEW-KEYS-AND-INPUT      PIC  X(03)  VALUE '182'.                 
023800   03    FEL-X3                  PIC  X(03)  VALUE '025'.                 
023900   03    FEL-X4                  PIC  X(03)  VALUE '183'.                 
024000   03    FEL-X5                  PIC  X(03)  VALUE '184'.                 
024100   03    SYSTEM-ERROR            PIC  X(03)  VALUE '099'.                 
024200   03    FEL-X7                  PIC  X(03)  VALUE '173'.                 
024300   03    FEL-X8                  PIC  X(03)  VALUE '174'.                 
024400   03    FEL-X9                  PIC  X(03)  VALUE '175'.                 
024500   03    FEL-XA                  PIC  X(03)  VALUE '176'.                 
024600   03    FEL-XB                  PIC  X(03)  VALUE '177'.                 
024700   03    FEL-XC                  PIC  X(03)  VALUE '152'.                 
024800   03    UNPACKED-CASES-EXIST    PIC  X(03)  VALUE '178'.                 
024900   03    FEL-XE                  PIC  X(03)  VALUE '179'.                 
025000   03    FEL-XF                  PIC  X(03)  VALUE '180'.                 
025100   03    LAST-ORDER-PART         PIC  X(03)  VALUE '181'.                 
025200   03    TOO-MANY-LINES          PIC  X(03)  VALUE '028'.                 
025300   03    FEL-XI                  PIC  X(03)  VALUE '170'.                 
025400   03    FEL-XJ                  PIC  X(03)  VALUE '171'.                 
025500   03    NEXT-ORDERPART-INDICATOR                                         
025600                                 PIC  X(03)  VALUE '172'.                 
025700   03    FEL-XL                  PIC  X(03)  VALUE '205'.                 
025800     EJECT                                                                
025900 01    FILLER                    PIC X(16)   VALUE 'TRANS-AREA '.         
026000 01    4397-TRANSAREA.                                                    
026100   03    4397-IDPRODNR           PIC 9(7)    VALUE ZERO.                  
026200   03    4397-IDANSTNR           PIC 9(5)    VALUE ZERO.                  
026300   03    4397-IDPLKLST           PIC 9(3)    VALUE ZERO.                  
026400   03    4397-IDPURAD            PIC 9(5)    VALUE ZERO.                  
026500   03    FILLER                  PIC X(80)   VALUE SPACE.                 
026600     EJECT                                                                
026700 01    FILLER                    PIC X(16)   VALUE 'ALT-IO-AREA'.         
026800 01    ALT-IO-AREA.                                                       
026900   03    ALT-LL                  PIC S9(4)   COMP  SYNC.                  
027000   03    ALT-Z1                  PIC X(1).                                
027100   03    ALT-Z2                  PIC X(1).                                
027200   03    ALT-TRANSKOD            PIC X(8)    VALUE SPACE.                 
027300   03    ALT-IDTRANS             PIC X(4)    VALUE SPACE.                 
027400   03    ALT-KDMFSFOR            PIC X(1)    VALUE SPACE.                 
027500   03    ALT-AREA                PIC X(105)  VALUE SPACE.                 
027600     SKIP2                                                                
027700******************************************************************        
027800*                                                                         
027900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
028000*                                                                         
028100 01    IMS-WS.                                                            
028200   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
028300     SKIP3                                                                
028400*                        **** STATUS-KOD FRÅN IMS                         
028500   03    STATUS-KUNDORDER-SEK-WS PIC XX.                                  
028600     88    KUNDORDER-SEK-FINNS               VALUE '  '.                  
028700     88    KUNDORDER-SEK-SAKNAS              VALUE 'GE' 'GB'.             
028800   03    STATUS-ORAD-WS          PIC XX.                                  
028900     88    ORAD-FINNS                        VALUE '  '.                  
029000     88    ORAD-SAKNAS                       VALUE 'GE'.                  
029100   03    STATUS-WS               PIC XX.                                  
029200     88    SEGMENT-FINNS                     VALUE '  '.                  
029300     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
029400     88    SEGMENT-FINNS-REDAN               VALUE 'II'.                  
029500     SKIP3                                                                
029600   03    GODK-STATUSKODER.                                                
029700     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
029800     SKIP3                                                                
029900 01    SSA1                      PIC X(64).                               
030000 01    SSA2                      PIC X(64).                               
030100 01    SSA3                      PIC X(64).                               
030200 01    SSA4                      PIC X(64).                               
030300     EJECT                                                                
030400*                            IMS FUNKTIONSKODER                           
030500*01    -COPY W0003                                                        
030600     EJECT                                                                
030700*                            DLI INPUT-OUTPUT AREA                        
030800 01    DLI-IO-AREA.                                                       
030900   03    IO-AREA                 PIC X(550)  VALUE SPACE.                 
031000     SKIP3                                                                
031100*  03    WDE401 -COPY WDE401               -RED IO-AREA.                  
031200     EJECT                                                                
031300*  03    WDE411 -COPY WDE411               -RED IO-AREA.                  
031400     EJECT                                                                
031500*  03    WDE601 -COPY WDE601               -RED IO-AREA.                  
031600     EJECT                                                                
031700                                                                          
031800     EJECT                                                                
031900 LINKAGE SECTION.                                                         
032000*01  MSG-PCB                     PIC X.                                   
032100*01    -COPY W0009     -PRE MSG-                                          
032200     EJECT                                                                
032300*01    -COPY W0009     -PRE ALT-                                          
032400     EJECT                                                                
      *01    -COPY W0009     -PRE SYNQ-                                         
           EJECT                                                                
032800*01    -COPY W0008     -PRE WDE4-                                         
032900     05  FILLER                  PIC X.                                   
033000     EJECT                                                                
033100*01    -COPY W0008     -PRE WDE42-                                        
033200     05  FILLER                  PIC X.                                   
033300     EJECT                                                                
033400*01    -COPY W0008     -PRE WDE43-                                        
033500     05  FILLER                  PIC X.                                   
033600     EJECT                                                                
033700*01    -COPY W0008     -PRE WDE6-                                         
033800     05  FILLER                  PIC X.                                   
       01  SYNQ-ATAB-PCB             PIC X.                                     
       01  WDQ3-PCB                  PIC X.                                     
033900                                                                          
034000     EJECT                                                                
034100 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB SYNQ-PCB                       
                                 WDE4-PCB WDE42-PCB                             
034200                           WDE43-PCB WDE6-PCB                             
                                 SYNQ-ATAB-PCB WDQ3-PCB.                        
034300 MAIN SECTION.                                                            
034400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB SYNQ-PCB                       
                                 WDE4-PCB WDE42-PCB                             
034500                           WDE43-PCB WDE6-PCB                             
034600                           SYNQ-ATAB-PCB WDQ3-PCB.                        
034700     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
034800     IF SUB-KDRC = 0                                                      
034900      IF REQU-KDPGMACT = 'E' OR 'S'                                       
035000        PERFORM A-INIT                                                    
035100        PERFORM B-GENERELL-KONTROLL                                       
035200        IF WS-INDATA-RATT                                                 
035300           PERFORM C-RELATIONSKONTROLL                                    
035400           IF WS-INDATA-RATT                                              
035500                EVALUATE TRUE                                             
035600                WHEN REQU-L123-FLSVAR = YES OR JA                         
035700                  PERFORM D-KONTROLL-OK-JA                                
035800                WHEN REQU-L123-FLSVAR = NEJ                               
035900                  PERFORM E-KONTROLL-OK-NEJ                               
036000                WHEN OTHER                                                
036100                  PERFORM F-KONTROLL-OK-BLANK                             
036200                END-EVALUATE                                              
036300           END-IF                                                         
036400        END-IF                                                            
036500        IF WS-INDATA-RATT                                                 
036600           PERFORM H-AVSLUT                                               
036700        END-IF                                                            
036800      ELSE                                                                
036900         MOVE SYSTEM-ERROR      TO RESP-IDMSG-ERROR                       
037000      END-IF                                                              
037100       MOVE RESP-IDMSG-INFO    TO WS-IDMSG-INFO                           
037200       MOVE RESP-IDMSG-ERROR   TO WS-IDMSG-ERROR                          
037300       MOVE RESP-IDELMT-ERROR  TO WS-IDELMT-ERROR                         
037400       IF WS-IDMSG-ERROR NOT = SPACE                                      
037500           MOVE ALL '+' TO RESP-WL0123O1(1:34)                            
037600           MOVE WS-IDMSG-ERROR   TO RESP-IDMSG-ERROR                      
037700           MOVE WS-IDELMT-ERROR  TO RESP-IDELMT-ERROR                     
037800           MOVE WS-IDMSG-INFO    TO RESP-IDMSG-INFO                       
037900           MOVE 001              TO RESP-IDMSGVER                         
038000           MOVE WS-COUNT         TO RESP-KVRADER                          
038100       END-IF                                                             
038200                                                                          
038300       PERFORM S02-RETURN-RESPONSE                                        
038400     END-IF                                                               
038500     MOVE ZERO TO RETURN-CODE                                             
038600     GOBACK                                                               
038700     .                                                                    
038800     EJECT                                                                
038900 A-INIT SECTION.                                                          
039000                                                                          
039100                                                                          
039200     MOVE ALL '+'                         TO RESP-AREA                    
039300     MOVE 001                             TO RESP-IDMSGVER                
039400     MOVE SPACE                           TO RESP-IDMSG-ERROR             
039500                                             RESP-IDMSG-INFO              
039600                                             RESP-IDELMT-ERROR            
039700     MOVE ZERO                            TO WS-COUNT                     
039800     MOVE ZERO                            TO RESP-KVRADER                 
039900     MOVE RAETT                           TO WS-INDATA-TEST               
040000                                             WS-BEHANDLING-TEST           
040100     MOVE NEJ                             TO WS-4397-STARTAD-SW           
040200                                                                          
040300     PERFORM AA-FLYTTA-NYCKLAR                                            
040400     PERFORM AB-SATT-FLAGGA-NYA-NYCKLAR                                   
040500                                                                          
040600     .                                                                    
040700     EJECT                                                                
040800 AA-FLYTTA-NYCKLAR  SECTION.                                              
040900                                                                          
041000                                                                          
041100     IF REQU-L123-IDANSTNR-KEY = ALL '+'                                  
041200      CONTINUE                                                            
041300     ELSE                                                                 
041400         MOVE REQU-L123-IDANSTNR-KEY      TO   WS-IDANSTNR                
041500     END-IF                                                               
041600     SKIP2                                                                
041700     IF REQU-L123-IDPRODNR-KEY = ALL '+'                                  
041800        MOVE ZERO                         TO  WS-IDPRODNR                 
041900     ELSE                                                                 
042000         MOVE REQU-L123-IDPRODNR-KEY      TO   WS-IDPRODNR                
042100     END-IF                                                               
042200     SKIP2                                                                
042300     IF REQU-L123-IDDISTR-KEY = ALL '+'                                   
042400        MOVE ZERO                         TO WS-IDDISTR                   
042500     ELSE                                                                 
042600         MOVE REQU-L123-IDDISTR-KEY       TO   WS-IDDISTR                 
042700     END-IF                                                               
042800     SKIP2                                                                
042900     IF REQU-L123-IDKUNDNR-KEY = ALL '+'                                  
043000       MOVE ZERO                       TO   WS-IDKUNDNR                   
043100     ELSE                                                                 
043200         MOVE REQU-L123-IDKUNDNR-KEY      TO   WS-IDKUNDNR                
043300     END-IF                                                               
043400     SKIP2                                                                
043500     IF REQU-L123-IDORDNR-KEY = ALL '+'                                   
043600        MOVE ZERO                         TO WS-IDORDNR                   
043700     ELSE                                                                 
043800         MOVE REQU-L123-IDORDNR-KEY       TO   WS-IDORDNR                 
043900     END-IF                                                               
044000     SKIP2                                                                
044100     IF  REQU-L123-IDKOLLI-KEY = ALL '+'                                  
044200       CONTINUE                                                           
044300     ELSE                                                                 
044400         MOVE REQU-L123-IDKOLLI-KEY       TO   WS-IDKOLLI                 
044500     END-IF                                                               
044600                                                                          
044700     IF  WS-IDANSTNR NUMERIC                                              
044800     MOVE WS-IDANSTNR                     TO   RESP-IDANSTNR-KEY          
044900     INSPECT RESP-IDANSTNR-KEY REPLACING LEADING ZERO BY SPACE            
045000     END-IF                                                               
045100                                                                          
045200     IF WS-IDDISTR NUMERIC                                                
045300     MOVE WS-IDDISTR                      TO   RESP-IDDISTR-KEY           
045400     INSPECT RESP-IDDISTR-KEY  REPLACING LEADING ZERO BY SPACE            
045500     END-IF                                                               
045600                                                                          
045700     IF WS-IDKUNDNR NUMERIC                                               
045800     MOVE WS-IDKUNDNR                     TO   RESP-IDKUNDNR-KEY          
045900     INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE            
046000     END-IF                                                               
046100                                                                          
046200     IF WS-IDORDNR NUMERIC                                                
046300     MOVE WS-IDORDNR                      TO   RESP-IDORDNR-KEY           
046400     INSPECT RESP-IDORDNR-KEY  REPLACING LEADING ZERO BY SPACE            
046500     END-IF                                                               
046600                                                                          
046700     IF WS-IDKOLLI NUMERIC                                                
046800     MOVE WS-IDKOLLI                      TO   RESP-IDKOLLI-KEY           
046900     INSPECT RESP-IDKOLLI-KEY  REPLACING LEADING ZERO BY SPACE            
047000     END-IF                                                               
047100                                                                          
047200     IF WS-IDPRODNR NUMERIC                                               
047300     MOVE WS-IDPRODNR                     TO   RESP-IDPRODNR-KEY          
047400     INSPECT RESP-IDPRODNR-KEY REPLACING LEADING ZERO BY SPACE            
047500     END-IF                                                               
047600                                                                          
047700     MOVE REQU-L123-IDDC-KEY              TO   RESP-IDDC-KEY              
047800                                                                          
047900     IF WS-IDPLKLST-SPAR = ZERO                                           
048000        MOVE 001                          TO   RESP-IDPLKLST-SPAR         
048100     ELSE                                                                 
048200        MOVE WS-IDPLKLST-SPAR             TO   RESP-IDPLKLST-SPAR         
048300     END-IF                                                               
048400     MOVE WS-IDRADNR-SPAR                 TO   RESP-IDRADNR-SPAR          
048500     .                                                                    
048600     EJECT                                                                
048700 AB-SATT-FLAGGA-NYA-NYCKLAR              SECTION.                         
048800                                                                          
048900        IF REQU-L123-IDPLKLST-SPAR NOT NUMERIC                            
049000          MOVE 001              TO REQU-L123-IDPLKLST-SPAR                
049100        END-IF                                                            
049200        IF REQU-L123-IDRADNR-SPAR  NOT NUMERIC                            
049300          MOVE 001              TO REQU-L123-IDRADNR-SPAR                 
049400        END-IF                                                            
049500     .                                                                    
049600     EJECT                                                                
049700 B-GENERELL-KONTROLL  SECTION.                                            
049800     SKIP3                                                                
049900     IF WS-IDANSTNR NOT NUMERIC                                           
050000     OR WS-IDANSTNR =   ZERO                                              
050100         MOVE FEL                       TO   WS-INDATA-TEST               
050200         MOVE 'IDANSTNR'                TO  RESP-IDELMT-ERROR             
050300         MOVE FEL-X1                    TO   RESP-IDMSG-ERROR             
050400     END-IF                                                               
050500     SKIP2                                                                
050600     IF WS-IDDISTR  NOT NUMERIC                                           
050700         MOVE FEL                       TO   WS-INDATA-TEST               
050800         MOVE 'IDDISTR'                TO  RESP-IDELMT-ERROR              
050900         MOVE FEL-X1                    TO   RESP-IDMSG-ERROR             
051000     ELSE                                                                 
051100         MOVE WS-IDDISTR                TO   W-401-IDDISTR                
051200     END-IF                                                               
051300     SKIP2                                                                
051400     IF WS-IDKUNDNR NOT NUMERIC                                           
051500         MOVE FEL                       TO   WS-INDATA-TEST               
051600         MOVE 'IDKUNDNR'               TO  RESP-IDELMT-ERROR              
051700         MOVE FEL-X1                    TO   RESP-IDMSG-ERROR             
051800     ELSE                                                                 
051900         MOVE WS-IDKUNDNR               TO   W-401-IDKUNDNR               
052000     END-IF                                                               
052100     SKIP2                                                                
052200     IF WS-IDORDNR  NOT NUMERIC                                           
052300         MOVE FEL                       TO   WS-INDATA-TEST               
052400         MOVE 'IDORDNR'                TO  RESP-IDELMT-ERROR              
052500         MOVE FEL-X1                    TO   RESP-IDMSG-ERROR             
052600     ELSE                                                                 
052700         MOVE WS-IDORDNR                TO   W-401-IDORDNR                
052800     END-IF                                                               
052900     SKIP2                                                                
053000     IF WS-IDPRODNR NOT NUMERIC                                           
053100         MOVE FEL                       TO   WS-INDATA-TEST               
053200         MOVE 'IDPRODNR'               TO  RESP-IDELMT-ERROR              
053300         MOVE FEL-X1                    TO   RESP-IDMSG-ERROR             
053400     END-IF                                                               
053500                                                                          
053600                                                                          
053700     IF REQU-L123-IDDC-KEY IS > SPACE                                     
053800       CONTINUE                                                           
053900     ELSE                                                                 
054000       MOVE FEL                           TO WS-INDATA-TEST               
054100         MOVE 'IDDC'                   TO  RESP-IDELMT-ERROR              
054200       MOVE FEL-X1                        TO RESP-IDMSG-ERROR             
054300     END-IF                                                               
054400                                                                          
054500       PERFORM BA-KOLL-REQU-L123-FLAGGA                                   
054600       PERFORM BB-KOLL-REQU-L123-FLSVAR                                   
054700     .                                                                    
054800     EJECT                                                                
054900 BA-KOLL-REQU-L123-FLAGGA SECTION.                                        
055000*REQU-L123-FLAGGA = JA BETYDER ATT MAN VILL SE NÄSTA PLOCKLISTA           
055100*OCH EV. OPACKADE RADER.                                                  
055200                                                                          
055300     EVALUATE TRUE                                                        
055400     WHEN REQU-L123-FLAGGA = ALL '+'                                      
055500         CONTINUE                                                         
055600     WHEN REQU-L123-FLAGGA = SPACE OR NEJ                                 
055700         CONTINUE                                                         
055800     WHEN REQU-L123-FLAGGA = 'X' OR JA OR YES                             
055900            IF REQU-L123-FLSVAR = 'J' OR 'Y'                              
056000               MOVE FEL                  TO WS-INDATA-TEST                
056100               MOVE FEL-XI               TO RESP-IDMSG-ERROR              
056200            ELSE                                                          
056300               MOVE '+'                        TO REQU-L123-FLSVAR        
056400            END-IF                                                        
056500     WHEN OTHER                                                           
056600         MOVE FEL                        TO WS-INDATA-TEST                
056700         MOVE NEXT-ORDERPART-INDICATOR   TO RESP-IDMSG-ERROR              
056800         CONTINUE                                                         
056900     END-EVALUATE                                                         
057000     .                                                                    
057100     EJECT                                                                
057200 BB-KOLL-REQU-L123-FLSVAR SECTION.                                        
057300* GODKÄNN EV. AVSLUT AV EN ORDERDEL.                                      
057400                                                                          
057500     EVALUATE TRUE                                                        
057600     WHEN REQU-L123-FLSVAR = ALL '+'                                      
057700         CONTINUE                                                         
057800     WHEN REQU-L123-FLSVAR = ALL ' '                                      
057900         CONTINUE                                                         
058000     WHEN REQU-L123-FLSVAR = 'J' OR 'Y' OR 'N'                            
058100            CONTINUE                                                      
058200     WHEN OTHER                                                           
058300         MOVE FEL                     TO WS-INDATA-TEST                   
058400         MOVE FEL-XJ                  TO RESP-IDMSG-ERROR                 
058500     END-EVALUATE                                                         
058600     .                                                                    
058700     EJECT                                                                
058800 C-RELATIONSKONTROLL  SECTION.                                            
058900     SKIP3                                                                
059000     MOVE WS-IDDISTR                    TO WS-IDDISTR-NUM                 
059100     MOVE WS-IDKUNDNR                   TO WS-IDKUNDNR-NUM                
059200                                                                          
059300     PERFORM CA-KONTROLLERA-KUNDORDNR                                     
059400     SKIP2                                                                
059500     IF WS-INDATA-RATT                                                    
059600         PERFORM CB-KONTROLLERA-PACKARE                                   
059700     END-IF                                                               
059800     IF WS-IDPLKLST-SPAR = ZERO                                           
059900        MOVE 001              TO RESP-IDPLKLST-SPAR                       
060000                                 WS-IDPLKLST-SPAR                         
060100     ELSE                                                                 
060200        MOVE WS-IDPLKLST-SPAR TO RESP-IDPLKLST-SPAR                       
060300     END-IF                                                               
060400     MOVE WS-IDRADNR-SPAR  TO RESP-IDRADNR-SPAR                           
060500     .                                                                    
060600     EJECT                                                                
060700 CA-KONTROLLERA-KUNDORDNR SECTION.                                        
060800     SKIP3                                                                
060900     IF REQU-L123-IDPRODNR-KEY = ALL '+'                                  
061000         IF    REQU-L123-IDDISTR-KEY = ALL '+'                            
061100           AND REQU-L123-IDKUNDNR-KEY = ALL '+'                           
061200           AND REQU-L123-IDORDNR-KEY = ALL '+'                            
061300             IF WS-IDPRODNR > ZERO                                        
061400*--------------------------ANVÄNDS GAMLA PRODNR: MID-IDPRODNR-UT          
061500                 MOVE JA              TO  SOEK-VIA-PRODNR                 
061600             ELSE                                                         
061700                 PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                        
061800             END-IF                                                       
061900         ELSE                                                             
062000             PERFORM CAA-HAMTA-PRODNR-I-WDE4-6                            
062100         END-IF                                                           
062200     ELSE                                                                 
062300         MOVE JA                       TO SOEK-VIA-PRODNR                 
062400     END-IF                                                               
062500     SKIP2                                                                
062600     IF WS-INDATA-RATT                                                    
062700         PERFORM CAB-KOLLA-MOT-WDE411                                     
062800     END-IF                                                               
062900     .                                                                    
063000     EJECT                                                                
063100 CAA-HAMTA-PRODNR-I-WDE4-6  SECTION.                                      
063200     SKIP3                                                                
063300     MOVE 'N'                             TO WS-SLINGA-KLAR               
063400                                                                          
063500     MOVE WS-IDDISTR-NUM                  TO W-4A1-IDDISTR                
063600     MOVE WS-IDKUNDNR-NUM                 TO W-4A1-IDKUNDNR               
063700     MOVE WS-IDORDNR                      TO W-4A1-IDORDNR                
063800     PERFORM IMS-GU-KUNDORDER-SEK                                         
063900                                                                          
064000     IF KUNDORDER-SEK-FINNS                                               
064100        PERFORM UNTIL KUNDORDER-SEK-SAKNAS OR                             
064200                      SLINGA-KLAR                                         
064300        MOVE KORD-IDDISTR                 TO W-401-IDDISTR                
064400        MOVE KORD-IDKUNDNR                TO W-401-IDKUNDNR               
064500        MOVE KORD-IDORDNR5                TO W-401-IDORDNR                
064600        MOVE KORD-IDPRODNR                TO W-401-IDPRODNR               
064700        MOVE KORD-IDPLKLST                TO W-401-IDPLKLST               
064800        PERFORM IMS-GU-KUNDORDER                                          
064900        MOVE KORD-KVORDRAD-LEVPL   TO WS-KVORDRAD-LEVPL-SPAR              
065000                                                                          
065100        MOVE KORD-IDPRODNR         TO W-601-IDPRODNR                      
065200        PERFORM IMS-GU-IDPRODNR                                           
065300        IF SEGMENT-FINNS                                                  
065400           IF SOEK-VIA-PRODNR = JA OR YES                                 
065500              MOVE VORD-IDPRODNR TO WS-JFR-IDPRODNR                       
065600              IF WS-JFR-IDPRODNR = WS-IDPRODNR                            
065700                 CONTINUE                                                 
065800              ELSE                                                        
065900                 SET SEGMENT-SAKNAS TO TRUE                               
066000              END-IF                                                      
066100           ELSE                                                           
066200              IF VORD-IDDC = REQU-L123-IDDC-KEY AND                       
066300                 WS-KVORDRAD-LEVPL-SPAR = ZERO                            
066400                 CONTINUE                                                 
066500              ELSE                                                        
066600                 SET SEGMENT-SAKNAS TO TRUE                               
066700              END-IF                                                      
066800           END-IF                                                         
066900        END-IF                                                            
067000        IF SEGMENT-FINNS                                                  
067100           IF VORD-IDDC = REQU-L123-IDDC-KEY                              
067200              MOVE 'J'                 TO   WS-SLINGA-KLAR                
067300              MOVE VORD-IDPRODNR       TO   WS-IDPRODNR                   
067400           END-IF                                                         
067500        END-IF                                                            
067600                                                                          
067700        PERFORM IMS-GN-KUNDORDER-SEK                                      
067800        END-PERFORM                                                       
067900     ELSE                                                                 
068000        MOVE FEL                     TO   WS-INDATA-TEST                  
068100        MOVE FEL-X3                  TO   RESP-IDMSG-ERROR                
068200        MOVE 'IDORDNR'               TO   RESP-IDELMT-ERROR               
068300     END-IF                                                               
068400                                                                          
068500     IF NOT SLINGA-KLAR                                                   
068600        MOVE FEL                         TO   WS-INDATA-TEST              
068700        MOVE FEL-X3                      TO   RESP-IDMSG-ERROR            
068800        MOVE 'IDORDNR'                   TO   RESP-IDELMT-ERROR           
068900     END-IF                                                               
069000     .                                                                    
069100     EJECT                                                                
069200 CAB-KOLLA-MOT-WDE411       SECTION.                                      
069300     SKIP3                                                                
069400     MOVE WS-IDPRODNR                   TO W-411-IDPRODNR-MIN             
069500                                           W-411-IDPRODNR-MAX             
069600     MOVE 1                             TO W-411-IDPURAD-MIN              
069700     MOVE 99999                         TO W-411-IDPURAD-MAX              
069800     PERFORM IMS-GU-KUNDORDER-SEK-INV                                     
069900                                                                          
070000     IF SEGMENT-FINNS                                                     
070100        IF KORD-IDDC = REQU-L123-IDDC-KEY                                 
070200          MOVE KORD-IDDISTR             TO   WS-IDDISTR-NUM               
070300          MOVE WS-IDDISTR-NUM           TO   WS-IDDISTR                   
070400                                               RESP-IDDISTR-KEY           
070500          INSPECT RESP-IDDISTR-KEY REPLACING LEADING ZERO BY SPACE        
070600          MOVE KORD-IDKUNDNR            TO   WS-IDKUNDNR-NUM              
070700          MOVE WS-IDKUNDNR-NUM          TO   WS-IDKUNDNR                  
070800                                               RESP-IDKUNDNR-KEY          
070900          INSPECT RESP-IDKUNDNR-KEY REPLACING LEADING                     
071000                                                ZERO BY SPACE             
071100          MOVE KORD-IDKUNDRF            TO   WS-IDKUNDRF                  
071200          MOVE WS-IDORDNR               TO   RESP-IDORDNR-KEY             
071300          INSPECT RESP-IDORDNR-KEY REPLACING LEADING ZERO BY SPACE        
071400        ELSE                                                              
071500           MOVE FEL                        TO   WS-INDATA-TEST            
071600           MOVE FEL-X3                     TO   RESP-IDMSG-ERROR          
071700           MOVE 'IDORDNR'                  TO   RESP-IDELMT-ERROR         
071800        END-IF                                                            
071900     ELSE                                                                 
072000        MOVE FEL                        TO   WS-INDATA-TEST               
072100        MOVE FEL-X3                     TO   RESP-IDMSG-ERROR             
072200        MOVE 'IDORDNR'                  TO   RESP-IDELMT-ERROR            
072300     END-IF                                                               
072400     .                                                                    
072500     EJECT                                                                
072600 CB-KONTROLLERA-PACKARE SECTION.                                          
072700     SKIP3                                                                
072800     MOVE 'N'             TO WS-TRAEFF-PACKARE                            
072900     MOVE ZERO            TO WS-ANT-ODEL-KVAR-ATT-BEHANDLA                
073000*    IF FL-NYA-NYCKLAR                                                    
073100*      MOVE ZERO          TO WS-IDPLKLST-FOM                              
073200*                            WS-IDRADNR-SPAR                              
073300*    ELSE                                                                 
073400         IF REQU-L123-FLAGGA = 'X' OR JA OR YES                           
073500           MOVE ZERO  TO WS-IDPLKLST-FOM                                  
073600         ELSE                                                             
073700           IF REQU-L123-IDPLKLST-SPAR = ALL '+'                           
073800             MOVE +001    TO WS-IDPLKLST-FOM                              
073900           ELSE                                                           
074000             COMPUTE WS-IDPLKLST-FOM = REQU-L123-IDPLKLST-SPAR - 1        
074100             END-COMPUTE                                                  
074200           END-IF                                                         
074300         END-IF                                                           
074400         MOVE ZERO    TO WS-IDRADNR-SPAR                                  
074500*    END-IF                                                               
074600     PERFORM S01-HAEMTA-PLKLST-EJ-KLAR                                    
074700     .                                                                    
074800     EJECT                                                                
074900 D-KONTROLL-OK-JA SECTION.                                                
075000     SKIP3                                                                
075100        PERFORM DA-EV-STARTA-4397                                         
075200     .                                                                    
075300     EJECT                                                                
075400 DA-EV-STARTA-4397                       SECTION.                         
075500                                                                          
075600     MOVE WS-IDDISTR       TO W-401-IDDISTR                               
075700     MOVE WS-IDKUNDNR      TO W-401-IDKUNDNR                              
075800     MOVE WS-IDORDNR       TO W-401-IDORDNR                               
075900     MOVE WS-IDPRODNR      TO W-401-IDPRODNR                              
076000     MOVE WS-IDPLKLST-SPAR TO W-401-IDPLKLST                              
076100     PERFORM IMS-GHU-KUNDORDER                                            
076200                                                                          
076300*    IF FL-GAMLA-NYCKLAR  AND                                             
076400*       KORD-KDPAKOLL = 0                                                 
076500*      MOVE FEL                TO WS-INDATA-TEST                          
076600*      MOVE FEL-X4             TO RESP-IDMSG-ERROR                        
076700*    ELSE                                                                 
076800       IF KORD-FLPAFEL = NEJ                                              
076900          PERFORM DAA-STARTA-4397                                         
077000       ELSE                                                               
077100          PERFORM DAB-RADER-KVAR-ATT-RAPPORTERA                           
077200          MOVE +0    TO KORD-KDPAKOLL                                     
077300          MOVE NEJ   TO KORD-FLPAFEL                                      
077400          PERFORM IMS-REPL-KUNDORDER                                      
077500       END-IF                                                             
077600*    END-IF                                                               
077700     .                                                                    
077800     EJECT                                                                
077900 DAA-STARTA-4397                         SECTION.                         
078000                                                                          
078100     MOVE +2                TO KORD-KDPAKOLL                              
078200     PERFORM IMS-REPL-KUNDORDER                                           
078300     PERFORM DAAA-SKAPA-BAKGRUNDTRANS                                     
078400                                                                          
078500     IF WS-IDPLKLST-NAESTA > 0                                            
078600       MOVE WS-IDPLKLST-NAESTA   TO WS-IDPLKLST-SPAR                      
078700                                    RESP-IDPLKLST-SPAR                    
078800       MOVE ZERO                 TO WS-IDRADNR-SPAR                       
078900       PERFORM S02-LAGG-UT-RADER                                          
079000     END-IF                                                               
079100     .                                                                    
079200     EJECT                                                                
079300 DAAA-SKAPA-BAKGRUNDTRANS  SECTION.                                       
079400     SKIP3                                                                
           MOVE ZERO TO W-411-IDPURAD                                           
           PERFORM IMS-GNP-RAD-KVAL                                             
           PERFORM UNTIL SEGMENT-SAKNAS                                         
            IF REQU-L123-IDDC-KEY= 11 AND                                       
               (ORAD-ADLAGOMR = 90 OR 98) AND                                   
               ORAD-KDRADSTA < 4                                                
             MOVE 'PCK' TO SYNQC-ORDERTYPE                                      
             MOVE WS-IDPLKLST-SPAR TO SYNQC-IDPLKLST                            
             MOVE WS-IDPRODNR TO SYNQC-IDPRODNR                                 
             MOVE ORAD-IDPURAD TO SYNQC-IDRADNR                                 
             CALL W488ORCN USING  SYNQC-W488ORCN SYNQ-PCB                       
                                SYNQ-ATAB-PCB WDQ3-PCB                          
            END-IF                                                              
            PERFORM IMS-GNP-RAD-OKVAL                                           
           END-PERFORM                                                          
                                                                                
079500     MOVE 'W4T397X '              TO  ALT-TRANSKOD                        
079600                                      ALT-LTERM-NAME                      
079700     MOVE WS-KDMFSFOR             TO  ALT-KDMFSFOR                        
079800     MOVE 'L123'                  TO  ALT-IDTRANS                         
079900     MOVE +117                    TO  ALT-LL                              
080000     MOVE WS-IDPRODNR             TO  4397-IDPRODNR                       
080100     MOVE WS-IDANSTNR             TO  4397-IDANSTNR                       
080200     MOVE WS-IDPLKLST-SPAR        TO  4397-IDPLKLST                       
080300     MOVE ZERO                    TO  4397-IDPURAD                        
080400     MOVE 4397-TRANSAREA          TO  ALT-AREA                            
080500     PERFORM IMS-INSERT-ALTMSG                                            
080600     MOVE JA                      TO WS-4397-STARTAD-SW                   
080700     .                                                                    
080800     EJECT                                                                
080900 DAB-RADER-KVAR-ATT-RAPPORTERA           SECTION.                         
081000                                                                          
081100     MOVE FEL-X5         TO RESP-IDMSG-ERROR                              
081200     MOVE FEL            TO WS-BEHANDLING-TEST                            
081300     MOVE FEL-X7         TO RESP-IDMSG-INFO                               
081400     .                                                                    
081500     EJECT                                                                
081600 E-KONTROLL-OK-NEJ SECTION.                                               
081700                                                                          
081800     MOVE WS-IDDISTR        TO W-401-IDDISTR                              
081900     MOVE WS-IDKUNDNR       TO W-401-IDKUNDNR                             
082000     MOVE WS-IDORDNR        TO W-401-IDORDNR                              
082100     MOVE WS-IDPRODNR       TO W-401-IDPRODNR                             
082200     MOVE REQU-L123-IDPLKLST-SPAR TO W-401-IDPLKLST                       
082300     PERFORM IMS-GHU-KUNDORDER-GODK-GE                                    
082400                                                                          
082500     IF SEGMENT-FINNS                                                     
082600       MOVE +0         TO KORD-KDPAKOLL                                   
082700       MOVE NEJ        TO KORD-FLPAFEL                                    
082800       PERFORM IMS-REPL-KUNDORDER                                         
082900     END-IF                                                               
083000                                                                          
083100     .                                                                    
083200     EJECT                                                                
083300 F-KONTROLL-OK-BLANK SECTION.                                             
083400     SKIP3                                                                
083500     PERFORM S02-LAGG-UT-RADER                                            
083600* -----------  FÖR ATT GE UPPLYSTA FÄLT   ----------------------          
083700     .                                                                    
083800     EJECT                                                                
083900 H-AVSLUT             SECTION.                                            
084000                                                                          
084100     IF ((REQU-L123-FLSVAR = JA OR YES OR NEJ) OR                         
084200         (REQU-L123-FLAGGA = JA OR YES OR 'X'))                           
084300         IF WS-BEHANDLING-RATT                                            
084400           IF ((REQU-L123-FLSVAR = JA OR YES)       AND                   
084500                WS-ANT-ODEL-KVAR-ATT-BEHANDLA < 2)  OR                    
084600                REQU-L123-FLSVAR = NEJ                                    
084700             IF  REQU-L123-FLSVAR = JA OR YES                             
084800                 MOVE FEL-X8              TO  RESP-IDMSG-INFO             
084900             ELSE                                                         
085000                 MOVE FEL-X9              TO  RESP-IDMSG-INFO             
085100             END-IF                                                       
085200           END-IF                                                         
085300         ELSE                                                             
085400             PERFORM HC-SAMMA-BILD                                        
085500         END-IF                                                           
085600     END-IF                                                               
085700                                                                          
085800     IF REQU-L123-FLSVAR = '+' OR SPACE                                   
085900       MOVE 'N'                    TO RESP-FLSVAR                         
086000     END-IF                                                               
086100     .                                                                    
086200     EJECT                                                                
086300 HC-SAMMA-BILD        SECTION.                                            
086400     SKIP3                                                                
086500     MOVE +1 TO BILD-RAD                                                  
086600     PERFORM UNTIL BILD-RAD NOT < MAX-ANT-RADER-PLUS-1                    
086700         ADD +1 TO BILD-RAD                                               
086800     END-PERFORM                                                          
086900     .                                                                    
087000     EJECT                                                                
087100 S01-HAEMTA-PLKLST-EJ-KLAR SECTION.                                       
087200                                                                          
087300     MOVE JA                       TO WS-IDPLKLST-SPAR-AR-HOGST           
087400                                      WS-IDPLKLST-NAESTA-AR-HOGST         
087500     MOVE WS-IDDISTR-NUM           TO W-4A1-IDDISTR                       
087600     MOVE WS-IDKUNDNR-NUM          TO W-4A1-IDKUNDNR                      
087700     MOVE WS-IDORDNR               TO W-4A1-IDORDNR                       
087800     PERFORM IMS-GU-KUNDORDER-SEK                                         
087900                                                                          
088000     PERFORM UNTIL KUNDORDER-SEK-SAKNAS                                   
088100                                                                          
088200       MOVE KORD-IDPRODNR              TO WS-JFR-IDPRODNR                 
088300       MOVE KORD-IDUSER                TO WS-JFR-IDANSTNR                 
088400       IF WS-IDPRODNR = WS-JFR-IDPRODNR AND                               
088500          WS-IDANSTNR = WS-JFR-IDANSTNR-5                                 
088600          MOVE 'J'                     TO WS-TRAEFF-PACKARE               
088700                                                                          
088800         IF KORD-KDPAKOLL = 0 OR 1                                        
088900                                                                          
089000          IF ( (KORD-KVORDRAD-PACK  < KORD-KVORDRAD)       OR             
089100               (KORD-KVORDRAD-LEVPL > 0                    AND            
089200                KORD-KVORDRAD-PACK  < KORD-KVORDRAD-LEVPL) )              
089300                                                                          
089400             ADD +1 TO WS-ANT-ODEL-KVAR-ATT-BEHANDLA                      
089500             PERFORM S01A-SPARA-UNDAN-PLOCKLISTNR                         
089600             PERFORM S01B-EV-VISAS-HOGSTA-PLKLST                          
089700          END-IF                                                          
089800         END-IF                                                           
089900       END-IF                                                             
090000       PERFORM IMS-GN-KUNDORDER-SEK                                       
090100     END-PERFORM                                                          
090200                                                                          
090300     PERFORM S01C-KOLL-OM-SISTA-PLOCKLISTA                                
090400     PERFORM S01D-EV-INGA-ODEL-KVAR-ATT-BEH                               
090500     .                                                                    
090600     EJECT                                                                
090700 S01A-SPARA-UNDAN-PLOCKLISTNR            SECTION.                         
090800                                                                          
090900     IF KORD-IDPLKLST    > WS-IDPLKLST-FOM  AND                           
091000        WS-IDPLKLST-SPAR = 0                                              
091100        MOVE KORD-IDPLKLST             TO WS-IDPLKLST-SPAR                
091200     END-IF                                                               
091300                                                                          
091400     IF (REQU-L123-FLSVAR   = JA OR YES)        AND                       
091500         KORD-IDPLKLST NOT  = WS-IDPLKLST-SPAR  AND                       
091600         WS-IDPLKLST-NAESTA = 0                                           
091700       MOVE KORD-IDPLKLST              TO WS-IDPLKLST-NAESTA              
091800     END-IF                                                               
091900     .                                                                    
092000     EJECT                                                                
092100 S01B-EV-VISAS-HOGSTA-PLKLST             SECTION.                         
092200                                                                          
092300     IF WS-IDPLKLST-SPAR > 0                    AND                       
092400        WS-IDPLKLST-SPAR NOT = KORD-IDPLKLST                              
092500       MOVE NEJ                    TO WS-IDPLKLST-SPAR-AR-HOGST           
092600     END-IF                                                               
092700                                                                          
092800     IF WS-IDPLKLST-NAESTA > 0                  AND                       
092900        WS-IDPLKLST-NAESTA NOT = KORD-IDPLKLST                            
093000       MOVE NEJ                    TO WS-IDPLKLST-NAESTA-AR-HOGST         
093100     END-IF                                                               
093200     .                                                                    
093300     EJECT                                                                
093400 S01C-KOLL-OM-SISTA-PLOCKLISTA SECTION.                                   
093500                                                                          
093600     IF WS-IDPLKLST-SPAR = ZERO   AND                                     
093700        WS-ANT-ODEL-KVAR-ATT-BEHANDLA > 0                                 
093800       MOVE REQU-L123-IDPLKLST-SPAR TO WS-IDPLKLST-SPAR                   
093900                                                                          
094000       IF WS-IDPLKLST-SPAR = WS-IDPLKLST-NAESTA                           
094100         MOVE ZERO TO WS-IDPLKLST-NAESTA                                  
094200       END-IF                                                             
094300     END-IF                                                               
094400     .                                                                    
094500     EJECT                                                                
094600 S01D-EV-INGA-ODEL-KVAR-ATT-BEH          SECTION.                         
094700                                                                          
094800     IF WS-TRAEFF-PACKARE = NEJ                                           
094900*-------------------------------------------SAKNAS ANGIVEN                
095000*-------------------------------------------PACKARE PÅ ORDERN             
095100        MOVE FEL                       TO   WS-INDATA-TEST                
095200        MOVE FEL-XA                    TO   RESP-IDMSG-ERROR              
095300     ELSE                                                                 
095400        IF WS-ANT-ODEL-KVAR-ATT-BEHANDLA = 0                              
095500*----------------------------------------ÄR ANGIVEN PACKARES              
095600*----------------------------------------ORDERDELAR REDAN KLARA           
095700          MOVE FEL                     TO   WS-INDATA-TEST                
095800          MOVE FEL-XB                  TO   RESP-IDMSG-INFO               
095900        END-IF                                                            
096000     END-IF                                                               
096100     .                                                                    
096200     EJECT                                                                
096300 S02-LAGG-UT-RADER      SECTION.                                          
096400     SKIP3                                                                
096500     IF REQU-L123-IDPLKLST-SPAR NOT = ZERO AND                            
096600        REQU-L123-IDPLKLST-SPAR NOT = WS-IDPLKLST-SPAR                    
096700        IF WS-4397-STARTAD                                                
096800            CONTINUE                                                      
096900         ELSE                                                             
097000            PERFORM S02A-AATERST-TIDIGARE-PLKLST                          
097100        END-IF                                                            
097200     END-IF                                                               
097300                                                                          
097400     MOVE NEJ                           TO WS-SLINGA-KLAR                 
097500                                                                          
097600     MOVE +1                            TO   RAD-INX                      
097700                                             INX                          
097800                                                                          
097900     MOVE WS-IDDISTR-NUM                TO W-401-IDDISTR                  
098000     MOVE WS-IDKUNDNR-NUM               TO W-401-IDKUNDNR                 
098100     MOVE WS-IDORDNR                    TO W-401-IDORDNR                  
098200     MOVE WS-IDPRODNR                   TO W-401-IDPRODNR                 
098300     MOVE WS-IDPLKLST-SPAR              TO W-401-IDPLKLST                 
098400                                                                          
098500     IF WS-IDPLKLST-SPAR = ZERO                                           
098600       MOVE +001            TO W-401-IDPLKLST                             
098700     END-IF                                                               
098800                                                                          
098900     PERFORM IMS-GHU-KUNDORDER                                            
099000                                                                          
099100     IF KORD-KDPAKOLL = 2                                                 
099200        MOVE FEL                   TO   WS-INDATA-TEST                    
099300        MOVE FEL-XC                TO   RESP-IDMSG-ERROR                  
099400     ELSE                                                                 
099500       IF KORD-KDPAKOLL = ZERO                                            
099600          PERFORM S02B-KONTROLL                                           
099700       END-IF                                                             
099800     END-IF                                                               
099900                                                                          
100000     IF WS-INDATA-RATT                                                    
100100       MOVE KORD-IDPLKLST           TO RESP-IDPLKLST                      
100200                                       WS-IDPLKLST                        
100300       INSPECT RESP-IDPLKLST REPLACING LEADING ZERO BY SPACE              
100400       IF WS-IDRADNR-SPAR > ZERO                                          
100500          MOVE WS-IDRADNR-SPAR      TO W-411-IDPURAD                      
100600          PERFORM IMS-GNP-RAD-KVAL                                        
100700       ELSE                                                               
100800          PERFORM IMS-GNP-RAD-OKVAL                                       
100900       END-IF                                                             
101000                                                                          
101100       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
101200                     RAD-INX NOT < MAX-ANT-RADER-PLUS-1                   
101300         MOVE ORAD-IDPURAD                 TO WS-IDRADNR-SPAR             
101400                                                                          
101500         IF ORAD-KDRADSTA < 4                                             
101600            PERFORM S02C-BEHANDLA-EJ-PACKADE-RADER                        
101700            PERFORM S02D-FLYTTA-RADER-TILL-MOD                            
101800                                                                          
101900          IF RESP-FLNOLLJ (RAD-INX) = 'N'                                 
102000             MOVE FEL-XL                TO                                
102100                            RESP-IDMSG-ERROR-LINE  (RAD-INX)              
102200                            RESP-IDMSG-ERROR                              
102300          END-IF                                                          
102400                                                                          
102500            ADD +1              TO RAD-INX                                
102600                                   WS-COUNT                               
102700         END-IF                                                           
102800                                                                          
102900         ADD +1                 TO W-411-IDPURAD                          
103000                                                                          
103100                                                                          
103200         PERFORM IMS-GNP-RAD-OKVAL                                        
103300       END-PERFORM                                                        
103400                                                                          
103500       MOVE WS-COUNT            TO RESP-KVRADER                           
103600       IF WS-COUNT      < 500                                             
103700                                                                          
103800*         -- LÄGG TILLBAKA OM VI HITTAR FELET MED BORTTAGEN 4301          
103900*         -- MVH KA (05-01-19 KL 18:52)                                   
104000*         PERFORM S02G-KOLL-OAVSLUTADE-KOLLI                              
104100                                                                          
104200          PERFORM S02E-LAEGG-UT-MEDDELANDEN                               
104300                                                                          
104400          IF ORAD-FINNS                                                   
104500             MOVE WS-IDRADNR-SPAR           TO RESP-IDRADNR-SPAR          
104600          ELSE                                                            
104700             MOVE ZERO                      TO RESP-IDRADNR-SPAR          
104800          END-IF                                                          
104900                                                                          
105000       ELSE                                                               
105100         MOVE TOO-MANY-LINES  TO RESP-IDMSG-ERROR                         
105200       END-IF                                                             
105300     END-IF                                                               
105400     .                                                                    
105500     EJECT                                                                
105600 S02A-AATERST-TIDIGARE-PLKLST SECTION.                                    
105700     SKIP3                                                                
105800     MOVE WS-IDDISTR-NUM    TO W-401-IDDISTR                              
105900     MOVE WS-IDKUNDNR-NUM   TO W-401-IDKUNDNR                             
106000     MOVE WS-IDORDNR        TO W-401-IDORDNR                              
106100     MOVE WS-IDPRODNR       TO W-401-IDPRODNR                             
106200     MOVE REQU-L123-IDPLKLST-SPAR TO W-401-IDPLKLST                       
106300     PERFORM IMS-GHU-KUNDORDER-GODK-GE                                    
106400                                                                          
106500     IF SEGMENT-FINNS                                                     
106600       MOVE +0         TO KORD-KDPAKOLL                                   
106700       MOVE NEJ        TO KORD-FLPAFEL                                    
106800       PERFORM IMS-REPL-KUNDORDER                                         
106900     END-IF                                                               
107000     .                                                                    
107100     EJECT                                                                
107200 S02B-KONTROLL SECTION.                                                   
107300     SKIP3                                                                
107400*E'TRACKER 5522850 DT 20070917                                            
107410*-- OM MAN HOPPAR TILL ANNAN BILD UTAN J/N I KLARFLAGGAN, GÅR DET         
107420*-- INTE ATT PACK.RAPP. PÅ T.EX 4315 OM DET ÄR 1 HÄR.                     
107500***  MOVE 1              TO   KORD-KDPAKOLL                               
107600     MOVE NEJ            TO   KORD-FLPAFEL                                
107700     PERFORM IMS-REPL-KUNDORDER                                           
107800     .                                                                    
107900     EJECT                                                                
108000 S02C-BEHANDLA-EJ-PACKADE-RADER SECTION.                                  
108100                                                                          
108200     COMPUTE WS-KVORAPP = ORAD-KVAVBART - ORAD-KVLEVART                   
108300     MOVE ORAD-FLNOLLJ              TO WS-FLNOLLJ                         
108400     .                                                                    
108500     EJECT                                                                
108600 S02D-FLYTTA-RADER-TILL-MOD SECTION.                                      
108700                                                                          
108800     MOVE ORAD-IDPURAD   TO WS-IDRADNR-RED                                
108900     MOVE WS-IDRADNR-RED TO RESP-IDRADNR (RAD-INX)                        
109000     MOVE WS-KVORAPP     TO WS-KVORAPP-RED                                
109100     MOVE WS-KVORAPP-RED TO RESP-KVORAPP (RAD-INX)                        
109200     MOVE ORAD-ADLAGOMR  TO WS-ADLAGOMR-N                                 
109300     MOVE WS-ADLAGOMR    TO RESP-ADLAGOMR (RAD-INX)                       
109400     IF WS-FLNOLLJ = JA                                                   
109500       MOVE 'Y'          TO RESP-FLNOLLJ (RAD-INX)                        
109600     ELSE                                                                 
109700       MOVE WS-FLNOLLJ   TO RESP-FLNOLLJ (RAD-INX)                        
109800     END-IF                                                               
109900     .                                                                    
110000     EJECT                                                                
110100 S02E-LAEGG-UT-MEDDELANDEN SECTION.                                       
110200                                                                          
110300     IF KOLLIFEL                                                          
110400        PERFORM S02EA-UPPDATERA-FLPAFEL                                   
110500     END-IF                                                               
110600     PERFORM S02EB-KONTROLL-FLER-ORDERDELAR                               
110700     PERFORM S02EC-SATT-MEDDELANDE-TEXT                                   
110800     .                                                                    
110900     EJECT                                                                
111000 S02EA-UPPDATERA-FLPAFEL                 SECTION.                         
111100                                                                          
111200     MOVE WS-IDDISTR-NUM      TO W-401-IDDISTR                            
111300     MOVE WS-IDKUNDNR-NUM     TO W-401-IDKUNDNR                           
111400     MOVE WS-IDORDNR          TO W-401-IDORDNR                            
111500     MOVE WS-IDPRODNR         TO W-401-IDPRODNR                           
111600     MOVE WS-IDPLKLST-SPAR    TO W-401-IDPLKLST                           
111700                                                                          
111800     IF WS-IDPLKLST-SPAR = ZERO                                           
111900       MOVE WS-IDPLKLST       TO W-401-IDPLKLST                           
112000     END-IF                                                               
112100     PERFORM IMS-GHU-KUNDORDER                                            
112200                                                                          
112300     MOVE JA                  TO KORD-FLPAFEL                             
112400     PERFORM IMS-REPL-KUNDORDER                                           
112500     .                                                                    
112600     EJECT                                                                
112700 S02EB-KONTROLL-FLER-ORDERDELAR SECTION.                                  
112800     SKIP3                                                                
112900     MOVE NEJ                        TO WS-FLER-ORDERDELAR-FINNS          
113000                                                                          
113100     IF WS-ANT-ODEL-KVAR-ATT-BEHANDLA > 1  AND                            
113200        WS-IDPLKLST-SPAR NOT          = WS-IDPLKLST-NAESTA                
113300        MOVE JA                      TO WS-FLER-ORDERDELAR-FINNS          
113400     ELSE                                                                 
113500        IF WS-ANT-ODEL-KVAR-ATT-BEHANDLA > 2                              
113600           MOVE JA                   TO WS-FLER-ORDERDELAR-FINNS          
113700        END-IF                                                            
113800     END-IF                                                               
113900     .                                                                    
114000     EJECT                                                                
114100 S02EC-SATT-MEDDELANDE-TEXT              SECTION.                         
114200                                                                          
114300     IF RAD-INX NOT < MAX-ANT-RADER-PLUS-1                                
114400       CONTINUE                                                           
114500     ELSE                                                                 
114600       IF FLER-ORDERDELAR-FINNS                                           
114700         IF WS-IDPLKLST-SPAR = WS-IDPLKLST-NAESTA                         
114800           IF WS-IDPLKLST-NAESTA-AR-HOGST = JA                            
114900             MOVE FEL-XE                   TO RESP-IDMSG-INFO             
115000           ELSE                                                           
115100             MOVE FEL-XF                   TO RESP-IDMSG-INFO             
115200           END-IF                                                         
115300         ELSE                                                             
115400           IF WS-IDPLKLST-SPAR-AR-HOGST = JA                              
115500             MOVE FEL-XE                   TO RESP-IDMSG-INFO             
115600           ELSE                                                           
115700             MOVE FEL-XF                   TO RESP-IDMSG-INFO             
115800           END-IF                                                         
115900         END-IF                                                           
116000       ELSE                                                               
116100         MOVE LAST-ORDER-PART              TO RESP-IDMSG-INFO             
116200       END-IF                                                             
116300     END-IF                                                               
116400     .                                                                    
116500     EJECT                                                                
116600*    --- DISPATCHER SECTIONS                                              
116700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
116800                                                                          
116900     MOVE 'GETARG'               TO SUB-KDFUNC                            
117000     MOVE 'CARPARTS.LDC.CHECKNOTREPORTED'    TO SUB-ADDISPABS             
117100     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
117200                                                                          
117300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
117400                                                                          
117500     IF SUB-KDRC > 0                                                      
117600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
117700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
117800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
117900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
118000     END-IF                                                               
118100     .                                                                    
118200     SKIP3                                                                
118300 S02-RETURN-RESPONSE SECTION.                                             
118400                                                                          
118500     MOVE 'RETURN'                   TO SUB-KDFUNC                        
118600     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
118700                                                                          
118800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
118900                                                                          
119000     IF SUB-KDRC > 0                                                      
119100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
119200       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
119300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
119400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
119500     END-IF                                                               
119600     .                                                                    
119700     EJECT                                                                
119800*IMS                                                                      
119900 IMS-INSERT-ALTMSG SECTION.                                               
120000                                                                          
120100     MOVE LOW-VALUE TO ALT-Z1 ALT-Z2                                      
120200     MOVE SPACE TO GODK-STATUSKODER                                       
120300     CALL CBLTDLI USING ISRT ALT-PCB ALT-IO-AREA                          
120400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
120500     PERFORM IMS-STATUSKONTROLL                                           
120600     .                                                                    
120700     SKIP3                                                                
120800 IMS-GU-KUNDORDER-SEK SECTION.                                            
120900     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
121000            DELIMITED BY SIZE INTO SSA1                                   
121100     MOVE '  GE' TO GODK-STATUSKODER                                      
121200     CALL CBLTDLI USING GU     WDE42-PCB DLI-IO-AREA SSA1                 
121300     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
121400                               STATUS-KUNDORDER-SEK-WS                    
121500     PERFORM IMS-STATUSKONTROLL                                           
121600     .                                                                    
121700     SKIP2                                                                
121800 IMS-GN-KUNDORDER-SEK SECTION.                                            
121900     STRING 'WDE401  (WDE4ASEQ =' W-WDE4A1-KUNDORDER-X ')'                
122000            DELIMITED BY SIZE INTO SSA1                                   
122100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
122200     CALL CBLTDLI USING GN     WDE42-PCB DLI-IO-AREA SSA1                 
122300     MOVE WDE42-STATUS-CODE TO STATUS-WS                                  
122400                               STATUS-KUNDORDER-SEK-WS                    
122500     PERFORM IMS-STATUSKONTROLL                                           
122600     .                                                                    
122700     EJECT                                                                
122800 IMS-GU-KUNDORDER SECTION.                                                
122900     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
123000            DELIMITED BY SIZE INTO SSA1                                   
123100     MOVE '    ' TO GODK-STATUSKODER                                      
123200     CALL CBLTDLI USING GU     WDE4-PCB DLI-IO-AREA SSA1                  
123300     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
123400     PERFORM IMS-STATUSKONTROLL                                           
123500     .                                                                    
123600     SKIP2                                                                
123700 IMS-GHU-KUNDORDER SECTION.                                               
123800     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
123900            DELIMITED BY SIZE INTO SSA1                                   
124000     MOVE '    ' TO GODK-STATUSKODER                                      
124100     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-AREA SSA1                  
124200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
124300     PERFORM IMS-STATUSKONTROLL                                           
124400     .                                                                    
124500     SKIP2                                                                
124600 IMS-GHU-KUNDORDER-GODK-GE SECTION.                                       
124700     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
124800            DELIMITED BY SIZE INTO SSA1                                   
124900     MOVE 'GE  ' TO GODK-STATUSKODER                                      
125000     CALL CBLTDLI USING GHU    WDE4-PCB DLI-IO-AREA SSA1                  
125100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
125200     PERFORM IMS-STATUSKONTROLL                                           
125300     .                                                                    
125400     SKIP2                                                                
125500 IMS-GNP-RAD-KVAL SECTION.                                                
125600     STRING 'WDE401  (WDE401KY =' W-WDE401-KUNDORDER-X ')'                
125700            DELIMITED BY SIZE INTO SSA1                                   
125800     STRING 'WDE411  (IDPURAD  >' W-WDE411-IDPURAD-X ')'                  
125900            DELIMITED BY SIZE INTO SSA2                                   
126000     MOVE '  GE' TO GODK-STATUSKODER                                      
126100     CALL CBLTDLI USING GNP    WDE4-PCB DLI-IO-AREA SSA1 SSA2             
126200     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
126300                              STATUS-ORAD-WS                              
126400     PERFORM IMS-STATUSKONTROLL                                           
126500     .                                                                    
126600     SKIP2                                                                
126700 IMS-GNP-RAD-OKVAL SECTION.                                               
126800     MOVE   'WDE411'       TO   SSA1                                      
126900     MOVE '  GE' TO GODK-STATUSKODER                                      
127000     CALL CBLTDLI USING GNP    WDE4-PCB DLI-IO-AREA SSA1                  
127100     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
127200                              STATUS-ORAD-WS                              
127300     PERFORM IMS-STATUSKONTROLL                                           
127400     .                                                                    
127500     SKIP2                                                                
127600 IMS-REPL-KUNDORDER     SECTION.                                          
127700     MOVE '  ' TO GODK-STATUSKODER                                        
127800     CALL CBLTDLI USING REPL WDE4-PCB DLI-IO-AREA                         
127900     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
128000     PERFORM IMS-STATUSKONTROLL                                           
128100     .                                                                    
128200     SKIP2                                                                
128300 IMS-GU-IDPRODNR  SECTION.                                                
128400     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
128500            DELIMITED BY SIZE INTO SSA1                                   
128600     MOVE '  GE' TO GODK-STATUSKODER                                      
128700     CALL CBLTDLI USING GU     WDE6-PCB DLI-IO-AREA SSA1                  
128800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
128900     PERFORM IMS-STATUSKONTROLL                                           
129000     .                                                                    
129100     EJECT                                                                
129200 IMS-GU-KUNDORDER-SEK-INV SECTION.                                        
129300     STRING 'WDE411  (WDE4BSEQ>=' W-WDE4B-KEYSEQ-MIN-X ')'                
129400                    '&WDE4BSEQ<=' W-WDE4B-KEYSEQ-MAX-X ')'                
129500            DELIMITED BY SIZE INTO SSA1                                   
129600     MOVE 'WDE401' TO SSA2                                                
129700     MOVE '  GEGB'  TO GODK-STATUSKODER                                   
129800     CALL CBLTDLI USING GU    WDE43-PCB DLI-IO-AREA SSA1 SSA2             
129900     MOVE WDE43-STATUS-CODE TO STATUS-WS                                  
130000     PERFORM IMS-STATUSKONTROLL                                           
130100     .                                                                    
130200     EJECT                                                                
130300 IMS-STATUSKONTROLL SECTION.                                              
130400     SET STATUS-IX TO 1                                                   
130500     SEARCH GODK-STATUS AT END CALL FELLOG                                
130600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
130700     END-SEARCH                                                           
130800     .                                                                    
