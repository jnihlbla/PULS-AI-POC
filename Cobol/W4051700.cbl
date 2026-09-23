000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4051700.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   NOV 90.                                                  
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        MÖJLIGHETER: PROGRAMMET VISAR LASTADE OCH FAKTURERADE            
001200*                     ORDER, NÄR KDORDSTA = 5, SAMT RÄKNAR UT             
001300*                     ANTAL ARBETSDAGAR - TIDEN MELLAN REG.DATUM          
001400*                     OCH SISTA LASTNING.                                 
001500*                                                                         
001600*                     KUNDNR = 0 OCH/ELLER C-LAGER = 0 BETYDER            
001700*                     ATT PGM-ET VISAR ALLA KUNDER/ETT DISTR              
001800*                     RESP ALLA C-LAGER.                                  
001900*                                                                         
002000*                     PROGRAMMET TAR INTE HÄNSYN TILL DIREKTLEV           
002100*                     RADER.                                              
002200*                                                                         
002300*                     PROGRAMMET LÄSER: WDE6A                             
002400*                                       WDE6                              
002410*                                       WDE4E                             
002500*                                       WLORQI (WDQ2)                     
002600*                                       WDB6                              
002610*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W4T517                                              
002900*        MID:         W4I51701                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W4O51701                                            
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600 DATA DIVISION.                                                           
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003801*    -- CHECKED BY WY2000                                                 
003810     SKIP3                                                                
003900 77  IDPGM                   PIC X(8)    VALUE 'W4051700'.                
004000                                                                          
004100 77  JA                      PIC X       VALUE 'J'.                       
004200 77  NEJ                     PIC X       VALUE 'N'.                       
004300 77  ALLA                    PIC X       VALUE 'A'.                       
004400                                                                          
004500 77  SPRAK-IX                PIC S9(9)   VALUE +0   COMP SYNC.            
004600 77  INDX                    PIC S9(9)   VALUE +0   COMP SYNC.            
004700 77  MAX-INDX                PIC S9(9)   VALUE +14  COMP SYNC.            
004800                                                                          
004900 77  MAX-IDDC                PIC X(02)   VALUE '99'.                      
005000                                                                          
005200 77  WS-KDORDKL              PIC X       VALUE SPACE.                     
005300 77  WS-KDORDKL-NUM          PIC S9      VALUE ZERO COMP-3.               
005400 77  WS-IDPRODNR             PIC S9(7)   VALUE ZERO COMP-3.               
005500 77  WS-IDORDNR7-NUM         PIC 9(7)    VALUE ZERO.                      
005600 77  WS-IDKUNDRF             PIC X(10)   VALUE SPACE.                     
005700 77  WS-TILASTN-SK           PIC S9(7)   VALUE ZERO COMP-3.               
005800                                                                          
005810 77  WS-IDTIDZON             PIC X(2)    VALUE SPACE.                     
005820                                                                          
005900     EJECT                                                                
006000 77  W-IDTRANS               PIC X(4)    VALUE SPACE.                     
006100   88  EGEN-MID                          VALUE '4517'.                    
006200   88  GODK-MID                          VALUE '4511' '4512'              
006300                                               '4513' '4514'              
006400                                               '4515' '4516'              
006500                                               '4517' '4518'              
006600                                               '4519'.                    
006700                                                                          
006800 77  NYCKLAR-SW              PIC X       VALUE 'J'.                       
006900   88  NYCKLAR-OK                        VALUE 'J'.                       
007000   88  NYCKLAR-FEL                       VALUE 'N'.                       
007100                                                                          
007200 77  ALLT-SW                 PIC X       VALUE 'J'.                       
007300   88  ALLT-OK                           VALUE 'J'.                       
007400   88  ALLT-FEL                          VALUE 'N'.                       
007500                                                                          
007600 77  KUND-SW                 PIC X       VALUE 'A'.                       
007700   88  EN-KUND                           VALUE 'J'.                       
007800   88  ALLA-KUNDER                       VALUE 'A'.                       
007900                                                                          
008000 77  DC-SW                   PIC X       VALUE 'J'.                       
008100   88  EN-DC                             VALUE 'J'.                       
008200   88  ALLA-DC                           VALUE 'A'.                       
008300*      --- VALID IDDC CODES                                               
008400*                                                                         
008500*01    -COPY WWDCKONS                                                     
008600       EJECT                                                              
008800 77  IDDC-SW                 PIC X       VALUE 'J'.                       
008900   88  IDDC-RAETT                        VALUE 'J'.                       
009000   88  IDDC-FEL                          VALUE 'N'.                       
009100                                                                          
009200 77  KDORDKL-SW              PIC X       VALUE 'A'.                       
009300   88  EN-KDORDKL                        VALUE 'J'.                       
009400   88  ALLA-KDORDKL                      VALUE 'A'.                       
009500                                                                          
009600     EJECT                                                                
009700 01  FELM-CODES.                                                          
009800   03  FELM-URVAL-SAKNAS     PIC X(3)    VALUE '005'.                     
009900   03  FELM-SISTA-SIDAN-REDAN-VISAD                                       
010000                             PIC X(3)    VALUE '115'.                     
010100   03  FELM-DISTR-KUND-SAKNAS                                             
010200                             PIC X(3)    VALUE '040'.                     
010300 01  MESSAGE-CODES.                                                       
010400   03  INF-FOERSTA-SIDAN     PIC X(3)    VALUE '006'.                     
010500   03  INF-MER-INFO-FINNS    PIC X(3)    VALUE '105'.                     
010600   03  INF-FEL-NYCKEL        PIC X(3)    VALUE '401'.                     
010700                                                                          
010800 01  SPAR-AREA.                                                           
010900   03  SPAR-IDKUNDNR         PIC S9(7)   VALUE ZERO COMP-3.               
011000   03  SPAR-IDORDNR5         PIC 9(5)    VALUE ZERO.                      
011100   03  SPAR-IDKUNDRF         PIC X(10)   VALUE SPACE.                     
011200   03  SPAR-IDDC             PIC X(02).                                   
011300   03  SPAR-KDORDKL          PIC S9      VALUE ZERO COMP-3.               
011400   03  SPAR-TIREGDAT         PIC S9(7)   VALUE ZERO COMP-3.               
011500   03  SPAR-DABEGPAC         PIC  9(8)   VALUE ZERO.                      
011600   03  SPAR-TIPACKN-SK       PIC S9(7)   VALUE ZERO COMP-3.               
011700   03  SPAR-TIFAKT-SK        PIC S9(7)   VALUE ZERO COMP-3.               
011800   03  SPAR-DATRPAVT.                                                     
011900       05  SPAR-DATRPAVD     PIC  9(8)   VALUE ZERO.                      
012000       05  SPAR-TIHHMM       PIC S9(5)   VALUE ZERO COMP-3.               
012100   03  SPAR-TILASTN-SK       PIC S9(7)   VALUE ZERO COMP-3.               
012200   03  SPAR-KVARBDAG         PIC S9(3)   VALUE ZERO COMP-3.               
012300                                                                          
012400   03 SPAR-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.               
012500 EJECT                                                                    
012600 01  GENERELLA-SUBPROGRAM.                                                
012700   03  WMEDKONV              PIC X(8)    VALUE 'WMEDKONV'.                
012800   03  WORKDAY               PIC X(8)    VALUE 'WORKDAY '.                
012900   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
013000   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
013100   03  W005INIT              PIC X(8)    VALUE 'W005INIT'.                
013200     EJECT                                                                
013300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013400*01 -COPY WMSGINIT                                                        
013500     EJECT                                                                
013600*   -COPY WMEDAREA                                                        
013700     EJECT                                                                
013800*   -COPY WORKAREA                                                        
013900     EJECT                                                                
014000*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
014100   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
014200     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
014300                                                                          
014400 01  BILD-HOPP-AREOR.                                                     
014500                                                                          
014600   03    W-BILD               PIC X(4)    VALUE SPACE.                    
014700   03    W-HOPP-IDTRANS.                                                  
014800     05  FILLER               PIC X(1)    VALUE 'W'.                      
014900     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
015000     05  FILLER               PIC X(1)    VALUE 'T'.                      
015100     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
015200     05  FILLER               PIC X(2)    VALUE SPACE.                    
015300                                                                          
015400                                                                          
015500   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
015600   03      P-TO-P-SW.                                                     
015700                                                                          
015800     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
015900     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
016000     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
016100     05  P-TO-P-KDTRANS          PIC X(8).                                
016200     05  P-TO-P-IDTRANS          PIC X(4).                                
016300     05  P-TO-P-KDMFSFOR         PIC X(1).                                
016400     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
016500                                                                          
016600******************************************************************        
016700*                                                                         
016800*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
016900*                                                                         
017000 01  FILLER                  PIC X(16)   VALUE 'MFS-WS'.                  
017100     SKIP3                                                                
017200*01  MID -COPY W4I51701                                                   
017300     EJECT                                                                
017400*01  -COPY WMSGAREA                                                       
017500     EJECT                                                                
017600*  03  MOD -COPY W4O51701 -RED MSG-AREA.                                  
017700     EJECT                                                                
017800*01  -COPY WMFSAREA                                                       
017900     EJECT                                                                
018000******************************************************************        
018100*                                                                         
018200*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018300*                                                                         
018400 01  IMS-WS.                                                              
018500   03  FILLER                PIC X(16)   VALUE 'IMS-WS     '.             
018600     SKIP3                                                                
018700*                        **** STATUS-KOD FRÅN IMS                         
018800   03  STATUS-WS             PIC XX.                                      
018900     88  STATUS-OK                       VALUE '  '.                      
019000     88  SEGMENT-FINNS                   VALUE '  '.                      
019100     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
019200     88  BASEN-SLUT                      VALUE 'GB'.                      
019300     88  TRANSKOD-FEL                    VALUE 'A1'.                      
019400     88  SECURITY-FEL                    VALUE 'A4'.                      
019500     SKIP3                                                                
019600   03  GODK-STATUSKODER.                                                  
019700     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019800     SKIP3                                                                
019900 01  NYCKLAR-TILL-DLI.                                                    
020000                                                                          
020100*-----------------WDE6A                                                   
020200   03  W-WDE6A1KY-MIN-X.                                                  
020300     05  W-SEK-IDDISTR-MIN     PIC S9(5)                  COMP-3.         
020400     05  W-SEK-IDDC-MIN        PIC X(02).                                 
020500     05  W-SEK-KDFRAKT-MIN     PIC S9(3)   VALUE ZERO     COMP-3.         
020600     05  W-SEK-KDORDSTA-MIN    PIC S9      VALUE +5       COMP-3.         
020700     05  W-SEK-IDKUNDNR-MIN    PIC S9(7)                  COMP-3.         
020800     05  W-SEK-IDPRODNR-MIN    PIC S9(7)   VALUE ZERO     COMP-3.         
020900                                                                          
021000   03  W-SEK-KDORDSTA-X.                                                  
021100     05 W-SEK-KDORDSTA         PIC S9      VALUE +5    COMP-3.            
021200                                                                          
021300   03  W-SEK-IDKUNDNR-X.                                                  
021400     05 W-SEK-IDKUNDNR         PIC S9(7)               COMP-3.            
021500                                                                          
021600   03  W-SEK-IDPRODNR-X.                                                  
021700     05 W-SEK-IDPRODNR         PIC S9(7)               COMP-3.            
021800                                                                          
021900   03  W-WDE6A1KY-MAX-X.                                                  
022000     05  W-SEK-IDDISTR-MAX     PIC S9(5)                  COMP-3.         
022100     05  W-SEK-IDDC-MAX        PIC X(2).                                  
022200     05  W-SEK-KDFRAKT-MAX     PIC S9(3)   VALUE +999     COMP-3.         
022300     05  W-SEK-KDORDSTA-MAX    PIC S9      VALUE +5       COMP-3.         
022400     05  W-SEK-IDKUNDNR-MAX    PIC S9(7)                  COMP-3.         
022500     05  W-SEK-IDPRODNR-MAX    PIC S9(7)   VALUE +9999999 COMP-3.         
022600                                                                          
022700*-----------------WDE601                                                  
022800   03  W-WDE601-IDPRODNR-X.                                               
022900     05  W-VORD-IDPRODNR       PIC S9(7)   VALUE ZERO     COMP-3.         
023000                                                                          
023010*-----------------WDE4E1                                                  
023020   03  W-WDE4E1KY-MIN.                                                    
023030     05  W-E4-IDPRODNR-MIN     PIC S9(7)   COMP-3.                        
023040     05  FILLER                PIC X(19)   VALUE LOW-VALUE.               
023050                                                                          
023060   03  W-WDE4E1KY-MAX.                                                    
023070     05  W-E4-IDPRODNR-MAX     PIC S9(7)   COMP-3.                        
023080     05  FILLER                PIC X(19)   VALUE HIGH-VALUE.              
023090                                                                          
023100*-----------------WDQ2C1  SEKUNDÄRT INDEX                                 
023200   03  W-WDQ2CSEQ-X.                                                      
023300     05  W-SEQC-IDDISTR        PIC S9(5)   VALUE ZERO  COMP-3.            
023400     05  W-SEQC-IDKUNDNR       PIC S9(7)   VALUE ZERO  COMP-3.            
023500     05  W-SEQC-IDKUNDRF       PIC X(10)   VALUE SPACE.                   
023600                                                                          
023700*-----------------WDQ201                                                  
023800   03  W-WDQ201-IDORDER-X.                                                
023900     05  W-OHUV-IDORDER        PIC S9(7)   VALUE  ZERO COMP-3.            
024000                                                                          
024100*-----------------WDQ212                                                  
024200   03  W-WDQ212-IDDC-X.                                                   
024300     05  W-ARB-IDDC            PIC X(02).                                 
024310                                                                          
024320*-----------------WDB601                                                  
024400   03  W-IDDC-B6-X.                                                       
024410       05 W-IDDC-B6            PIC X(2).                                  
024420                                                                          
024500     SKIP2                                                                
024600 01    SSA1                  PIC X(144).                                  
024700 01    SSA2                  PIC X(96).                                   
024800     EJECT                                                                
024900*                            IMS FUNKTIONSKODER                           
025000*01    -COPY W0003                                                        
025100     EJECT                                                                
025200*                            DLI INPUT-OUTPUT AREA                        
025300 01  DLI-IO-AREA.                                                         
025400   03  IO-AREA               PIC X(4000) VALUE SPACE.                     
025500                                                                          
025600     SKIP2                                                                
026300*  03  WLORQL01     -COPY WDQ2C1             -RED IO-AREA.                
026400     EJECT                                                                
026500*  03  WLORQI01     -COPY WDQ201             -RED IO-AREA.                
026600     EJECT                                                                
026700*  03  WLORQI12     -COPY WDQ212             -RED IO-AREA.                
026800     EJECT                                                                
026801 01  FILLER           PIC X(16)    VALUE 'DLI-IO-WDE6A1'.                 
026802 01  DLI-IO-WDE6A1.                                                       
026810*  03  -COPY WDE6A1                                                       
026820     EJECT                                                                
026821 01  FILLER           PIC X(16)    VALUE 'DLI-IO-WDE601'.                 
026822 01  DLI-IO-WDE601.                                                       
026823*  03  -COPY WDE601                                                       
026824     EJECT                                                                
026825 01  FILLER           PIC X(16)    VALUE 'DLI-IO-WDE4E1'.                 
026826 01  DLI-IO-WDE4E1.                                                       
026830*  03  -COPY WDE4E1                                                       
026831                                                                          
026832 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
026833 01   DLI-IO-AREA-B601.                                                   
026834*     03  -COPY WDB601                                                    
026835                                                                          
026840     EJECT                                                                
026900                                                                          
027000 LINKAGE SECTION.                                                         
027100*01  -COPY W0009     -PRE MSG-                                            
027200     EJECT                                                                
027300*01  -COPY W0009     -PRE ALT-                                            
027400     EJECT                                                                
027500*01  -COPY W0008     -PRE USEA-                                           
027600     05  FILLER              PIC X.                                       
027700     EJECT                                                                
027800*01  -COPY W0008     -PRE WDE6A-                                          
027900     05  FILLER              PIC X.                                       
028000     EJECT                                                                
028100*01  -COPY W0008     -PRE WDE6-                                           
028200     05  FILLER              PIC X.                                       
028300     EJECT                                                                
028310*01  -COPY W0008     -PRE WDE4E-                                          
028320     05 FILLER               PIC X.                                       
028330     EJECT                                                                
028400*01  -COPY W0008     -PRE ORQI1-                                          
028500     05  FILLER              PIC X.                                       
028600*01  -COPY W0008     -PRE ORQI2-                                          
028700     05  FILLER              PIC X.                                       
028710*01  -COPY W0008     -PRE WDB6-                                           
028720     05  FILLER              PIC X.                                       
028800                                                                          
028900 EJECT                                                                    
029000 PROCEDURE DIVISION USING  MSG-PCB ALT-PCB USEA-PCB                       
029100                           WDE6A-PCB WDE6-PCB WDE4E-PCB                   
029200                           ORQI1-PCB ORQI2-PCB WDB6-PCB.                  
029300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
029400                           WDE6A-PCB WDE6-PCB WDE4E-PCB                   
029500                           ORQI1-PCB ORQI2-PCB WDB6-PCB.                  
029600                                                                          
029700     PERFORM IMS-GET-MSG                                                  
029800     IF SEGMENT-FINNS                                                     
029900       PERFORM A-INIT                                                     
030000       PERFORM B-KOLLA-NYCKLAR                                            
030100       IF NYCKLAR-OK                                                      
030200                                                                          
030300         IF MFS-FIRST                                                     
030400            PERFORM C-FOERSTA-SIDAN                                       
030500         ELSE                                                             
030600           IF MFS-NEXT                                                    
030700             PERFORM D-NAESTA-SIDAN                                       
030800           ELSE                                                           
030900             PERFORM E-SAMMA-SIDA                                         
031000           END-IF                                                         
031100         END-IF                                                           
031200                                                                          
031300         IF STARTA-ANNAN-BILD                                             
031400            CONTINUE                                                      
031500         ELSE                                                             
031600            IF ALLT-OK                                                    
031700              PERFORM F-LAES-VISA-INFO                                    
031800            END-IF                                                        
031900         END-IF                                                           
032000                                                                          
032100       END-IF                                                             
032200                                                                          
032300       IF STARTA-ANNAN-BILD                                               
032400          CONTINUE                                                        
032500       ELSE                                                               
032600          COMPUTE MSG-KVLL = LENGTH OF MOD-W4O51701 + 4                   
032700          PERFORM IMS-INSERT-MSG                                          
032800       END-IF                                                             
032900     END-IF                                                               
033000                                                                          
033100     MOVE ZERO TO RETURN-CODE                                             
033200     GOBACK                                                               
033300     .                                                                    
033400     EJECT                                                                
033500 A-INIT SECTION.                                                          
033600                                                                          
033700     IF MSG-DUBBLA-TRANSKODER                                             
033800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I51701                 
033900       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
034000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
034100     ELSE                                                                 
034200       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I51701                   
034300       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
034400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
034500     END-IF                                                               
034600                                                                          
034700     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
034800     MOVE MSG-IDPFK TO MFS-IDPFK                                          
034900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
035000                                                                          
035100     MOVE LOW-VALUE TO MSG-AREA                                           
035200     MOVE 'W4O517N1' TO MFS-IDMOD                                         
035300     MOVE '4517' TO MOD-IDTRANS                                           
035400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
035500                                                                          
035600     IF NOT EGEN-MID                                                      
035700       MOVE SPACE TO MFS-KDTRTYP                                          
035800       MOVE '7' TO MFS-IDPFK                                              
035900     END-IF                                                               
036000                                                                          
036100     .                                                                    
036200     EJECT                                                                
036300 B-KOLLA-NYCKLAR SECTION.                                                 
036400                                                                          
036500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
036600     MOVE '001'             TO MSGI-KDCALL                                
036700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
036710     MOVE '4517'            TO MSGI-IDTRANS                               
036720     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
036800     IF EGEN-MID                                                          
036900        MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                              
037000        MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                             
037200        MOVE MID-KDFRAKT-IN  TO MSGI-KDFRAKT                              
037300     END-IF                                                               
037400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
037410     MOVE MSGI-IDTIDZON     TO WS-IDTIDZON                                
037500     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
037600                                                                          
037700     MOVE JA TO NYCKLAR-SW                                                
037800                                                                          
037900     PERFORM BA-KOLLA-DISTRIKT                                            
038000     PERFORM BB-KOLLA-KUNDNR                                              
038100     PERFORM BC-KOLLA-DC                                                  
038200     PERFORM BD-KOLLA-ORDKLASS                                            
038300     PERFORM BE-BEHANDLA-FK-STATUS                                        
038400                                                                          
038500     IF NYCKLAR-FEL                                                       
038600       MOVE INF-FEL-NYCKEL TO MED-IDMFSFEL                                
038700       CALL WMEDKONV USING MED-WMEDAREA                                   
038800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
038900       PERFORM MFS-RENSA-FAELT-UT                                         
039000                                                                          
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 BA-KOLLA-DISTRIKT SECTION.                                               
039500                                                                          
039600     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
039700                                                                          
039800     IF MID-IDDISTR-IN     NOT = ALL '+'                                  
039900       MOVE '7'            TO MFS-IDPFK                                   
040000       MOVE SPACE          TO MFS-KDTRTYP                                 
040100     END-IF                                                               
040200                                                                          
040300     MOVE MSGI-IDDISTR TO MOD-IDDISTR-UT                                  
040400     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
040500                                                                          
040600     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
040700       MOVE MSGI-IDDISTR TO W-SEK-IDDISTR-MIN                             
040800                            W-SEK-IDDISTR-MAX                             
040900     ELSE                                                                 
041000       MOVE NEJ        TO NYCKLAR-SW                                      
041100     END-IF                                                               
041200     .                                                                    
041300     EJECT                                                                
041400 BB-KOLLA-KUNDNR SECTION.                                                 
041500                                                                          
041600     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
041700                                                                          
041800     IF MID-IDKUNDNR-IN   NOT = ALL '+'                                   
041900       MOVE '7'           TO MFS-IDPFK                                    
042000       MOVE SPACE         TO MFS-KDTRTYP                                  
042100     END-IF                                                               
042200                                                                          
042300     IF MSGI-IDKUNDNR = SPACE                                             
042400         MOVE +0          TO W-SEK-IDKUNDNR-MIN                           
042500         MOVE +9999999    TO W-SEK-IDKUNDNR-MAX                           
042600     ELSE                                                                 
042700       IF MSGI-IDKUNDNR NUMERIC                                           
042800         MOVE MSGI-IDKUNDNR TO W-SEK-IDKUNDNR-MIN                         
042900                               W-SEK-IDKUNDNR-MAX                         
043000                               W-SEK-IDKUNDNR                             
043100         MOVE JA            TO KUND-SW                                    
043200       ELSE                                                               
043300         MOVE NEJ         TO NYCKLAR-SW                                   
043400       END-IF                                                             
043500     END-IF                                                               
043600                                                                          
043700     MOVE MSGI-IDKUNDNR     TO MOD-IDKUNDNR-UT                            
043800     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
043900     IF MSGI-IDKUNDNR = ZERO                                              
044000       MOVE '     0'      TO MOD-IDKUNDNR-UT                              
044100     END-IF                                                               
044200     .                                                                    
044300     EJECT                                                                
044400 BC-KOLLA-DC  SECTION.                                                    
044500                                                                          
044600     MOVE MFS-RENSA-FAELT     TO MOD-IDDC-IN                              
044700                                                                          
044800     IF MID-IDDC-IN           = ALL '+'                                   
044900       MOVE MID-IDDC-UT       TO W-IDDC-B6                                
045100     ELSE                                                                 
045200       MOVE MID-IDDC-IN       TO W-IDDC-B6                                
045300       MOVE '7'               TO MFS-IDPFK                                
045400       MOVE SPACE             TO MFS-KDTRTYP                              
045600     END-IF                                                               
045610     PERFORM IMS-GU-WDB601                                                
045700                                                                          
045800     IF W-IDDC-B6 > SPACE                                                 
045900                                                                          
046000       IF W-IDDC-B6 NOT > MAX-IDDC                                        
046100                                                                          
046110         INSPECT W-IDDC-B6 REPLACING ALL SPACES BY ZEROS                  
046200         IF W-IDDC-B6 = ZERO                                              
046300           MOVE ALLA          TO DC-SW                                    
046400           MOVE WC-DC-ZERO    TO W-SEK-IDDC-MIN                           
046500                                 W-IDDC-B6                                
046510           PERFORM IMS-GU-WDB601                                          
046600           MOVE '99'          TO W-SEK-IDDC-MAX                           
046700         ELSE                                                             
046900           IF DCS-KDDC = SPACE OR DCS-CDC-TR                              
046910              MOVE MSGI-IDDC  TO W-SEK-IDDC-MIN                           
046920                                 W-SEK-IDDC-MAX                           
046930                                 W-IDDC-B6                                
046931              PERFORM IMS-GU-WDB601                                       
046940           ELSE                                                           
047000              MOVE DCS-IDDC   TO W-SEK-IDDC-MIN                           
047100                                 W-SEK-IDDC-MAX                           
047600           END-IF                                                         
047700         END-IF                                                           
047800       ELSE                                                               
047900         MOVE MSGI-IDDC  TO W-SEK-IDDC-MIN                                
048000                            W-SEK-IDDC-MAX                                
048100                            W-IDDC-B6                                     
048110         PERFORM IMS-GU-WDB601                                            
048200       END-IF                                                             
048300     ELSE                                                                 
048400       MOVE MSGI-IDDC         TO W-SEK-IDDC-MIN                           
048500                                 W-SEK-IDDC-MAX                           
048600                                 W-IDDC-B6                                
048610       PERFORM IMS-GU-WDB601                                              
048700     END-IF                                                               
048710                                                                          
048720     MOVE W-IDDC-B6       TO MOD-IDDC-UT                                  
048800     .                                                                    
048900     EJECT                                                                
049000 BD-KOLLA-ORDKLASS SECTION.                                               
049100                                                                          
049200     MOVE MFS-RENSA-FAELT TO MOD-KDORDKL-IN                               
049300                                                                          
049400     IF MID-KDORDKL-IN NOT = ALL '+'                                      
049500       MOVE '7'            TO MFS-IDPFK                                   
049600       MOVE SPACE          TO MFS-KDTRTYP                                 
049700       MOVE MID-KDORDKL-IN TO WS-KDORDKL                                  
049800     ELSE                                                                 
049900       IF MID-IDDISTR-IN  = ALL '+' AND                                   
050000          MID-IDKUNDNR-IN = ALL '+' AND                                   
050100          MID-IDDC-IN     = ALL '+'                                       
050200            MOVE MID-KDORDKL-UT TO WS-KDORDKL                             
050300       ELSE                                                               
050400            MOVE SPACE          TO WS-KDORDKL                             
050500       END-IF                                                             
050600     END-IF                                                               
050700                                                                          
050800     IF WS-KDORDKL = SPACE                                                
050900       MOVE ALLA             TO KDORDKL-SW                                
051000     ELSE                                                                 
051010       IF WS-KDORDKL NUMERIC AND WS-KDORDKL < 5                           
051020         MOVE WS-KDORDKL     TO WS-KDORDKL-NUM                            
051030         MOVE JA             TO KDORDKL-SW                                
051040       ELSE                                                               
051041         IF GODK-MID                                                      
051050            MOVE NEJ         TO NYCKLAR-SW                                
051051         ELSE                                                             
051052            MOVE ALLA        TO KDORDKL-SW                                
051053            MOVE SPACE       TO WS-KDORDKL                                
051054         END-IF                                                           
051060       END-IF                                                             
051070     END-IF                                                               
051080                                                                          
051090     MOVE WS-KDORDKL         TO MOD-KDORDKL-UT                            
051091     .                                                                    
051092     EJECT                                                                
051300 BE-BEHANDLA-FK-STATUS SECTION.                                           
051400                                                                          
051500     MOVE MFS-RENSA-FAELT TO MOD-KDFRAKT-IN                               
051600                             MOD-KDORDSTA-IN                              
051700                                                                          
051800     MOVE MSGI-KDFRAKT    TO MOD-KDFRAKT-UT                               
051900                                                                          
052000     IF MID-KDORDSTA-IN = ALL '+'                                         
052100       MOVE MID-KDORDSTA-UT TO MOD-KDORDSTA-UT                            
052200     ELSE                                                                 
052300       MOVE MID-KDORDSTA-IN TO MOD-KDORDSTA-UT                            
052400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700 C-FOERSTA-SIDAN SECTION.                                                 
052800                                                                          
052900     MOVE ZERO       TO MOD-IDKUNDNR-ENTER                                
053000                        MOD-IDPRODNR-ENTER                                
053100                        MOD-IDKUNDRF-ENTER                                
053200                                                                          
053300     MOVE ZERO       TO MOD-IDKUNDNR-NEXT                                 
053400                        MOD-IDPRODNR-NEXT                                 
053500                        MOD-IDKUNDRF-NEXT                                 
053600                                                                          
053700     MOVE INF-FOERSTA-SIDAN TO MED-IDMFSFEL                               
053800     CALL WMEDKONV USING MED-WMEDAREA                                     
053900     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
054000     .                                                                    
054100     EJECT                                                                
054200 D-NAESTA-SIDAN SECTION.                                                  
054300                                                                          
054400     IF MID-IDKUNDNR-NEXT = ZERO AND                                      
054500        MID-IDPRODNR-NEXT = ZERO AND                                      
054600        MID-IDKUNDRF-NEXT = ZERO                                          
054700                                                                          
054800       PERFORM MFS-RENSA-FAELT-UT                                         
054900                                                                          
055000       MOVE FELM-SISTA-SIDAN-REDAN-VISAD TO MED-IDMFSFEL                  
055100       CALL WMEDKONV USING MED-WMEDAREA                                   
055200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
055300                                                                          
055400       MOVE NEJ TO ALLT-SW                                                
055500                                                                          
055600     ELSE                                                                 
055700                                                                          
055800       MOVE MID-IDKUNDNR-NEXT TO W-SEK-IDKUNDNR-MIN                       
055900                                 W-SEK-IDKUNDNR                           
056000                                                                          
056100       MOVE MID-IDPRODNR-NEXT TO W-SEK-IDPRODNR-MIN                       
056200                                 W-SEK-IDPRODNR                           
056300                                                                          
056400     END-IF                                                               
056500     .                                                                    
056600     EJECT                                                                
056700 E-SAMMA-SIDA SECTION.                                                    
056800                                                                          
056900     IF MID-IDKUNDNR-ENTER = ZERO AND                                     
057000        MID-IDPRODNR-ENTER = ZERO AND                                     
057100        MID-IDKUNDRF-ENTER = ZERO                                         
057200                                                                          
057300       PERFORM MFS-RENSA-FAELT-UT                                         
057400       MOVE NEJ TO ALLT-SW                                                
057500                                                                          
057600     ELSE                                                                 
057700                                                                          
057800       MOVE MID-IDKUNDNR-ENTER TO W-SEK-IDKUNDNR-MIN                      
057900                                  W-SEK-IDKUNDNR                          
058000                                                                          
058100       MOVE MID-IDPRODNR-ENTER TO W-SEK-IDPRODNR-MIN                      
058200                                  W-SEK-IDPRODNR                          
058300                                                                          
058400     END-IF                                                               
058500                                                                          
058600     IF EGEN-MID                                                          
058700       MOVE +1                   TO INDX                                  
058800       PERFORM UNTIL INDX        >  MAX-INDX                              
058900          IF MID-IDTRANS(INDX)   =  ALL '+'                               
059000             CONTINUE                                                     
059100          ELSE                                                            
059200             IF MID-IDTRANS(INDX) NUMERIC                                 
059300                PERFORM EA-STARTA-ANNAN-BILD                              
059400                MOVE JA          TO SW-STARTA-ANNAN-BILD                  
059500                MOVE MAX-INDX    TO INDX                                  
059600             END-IF                                                       
059700          END-IF                                                          
059800          ADD +1                 TO INDX                                  
059900       END-PERFORM                                                        
060000     END-IF                                                               
060100     .                                                                    
060200     EJECT                                                                
060300                                                                          
060400 EA-STARTA-ANNAN-BILD  SECTION.                                           
060500                                                                          
060600     INSPECT MID-IDKUNDNR(INDX) REPLACING LEADING SPACE BY ZERO           
060700     MOVE MID-IDKUNDNR(INDX)     TO MSGI-IDKUNDNR                         
060800     INSPECT MID-IDORDNR7(INDX) REPLACING LEADING SPACE BY ZERO           
060900     MOVE MID-IDORDNR7(INDX)     TO MSGI-IDKUNDRF(1:7)                    
061000     MOVE '001'                  TO MSGI-KDCALL                           
061100     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
061200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
061300                                                                          
061400     MOVE LOW-VALUE              TO P-TO-P-KDZ1                           
061500     MOVE LOW-VALUE              TO P-TO-P-KDZ2                           
061600     MOVE MID-IDTRANS(INDX) (1:1)  TO W-HOPP-IDTRANS-2                    
061700     MOVE MID-IDTRANS(INDX) (2:3)  TO W-HOPP-IDTRANS-4-6                  
061800     MOVE W-HOPP-IDTRANS         TO P-TO-P-KDTRANS                        
061900     MOVE '4517'                 TO P-TO-P-IDTRANS                        
062000     MOVE MFS-KDMFSFOR           TO P-TO-P-KDMFSFOR                       
062100                                                                          
062200     PERFORM S01-INSERT-ALTMSG                                            
062300     .                                                                    
062400     EJECT                                                                
062500 F-LAES-VISA-INFO SECTION.                                                
062600                                                                          
062700     IF MFS-FIRST                                                         
062800       IF EN-KUND                                                         
062900         PERFORM IMS-GU-WDE6A1-DISTR-KUND                                 
063000       ELSE                                                               
063100         PERFORM IMS-GU-WDE6A1-DISTR-OKVAL                                
063200       END-IF                                                             
063300     ELSE                                                                 
063400       PERFORM IMS-GU-WDE6A1-DISTR-UNIK                                   
063500     END-IF                                                               
063600                                                                          
063700     IF SEGMENT-FINNS                                                     
063800                                                                          
063900       PERFORM FA-LAES-BEHANDLA-RADINFO                                   
064000                                                                          
064100       IF INDX = +1                                                       
064200         MOVE FELM-URVAL-SAKNAS TO MED-IDMFSFEL                           
064300         CALL WMEDKONV USING MED-WMEDAREA                                 
064400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
064500       END-IF                                                             
064600                                                                          
064700     ELSE                                                                 
064800                                                                          
064900       MOVE FELM-DISTR-KUND-SAKNAS TO MED-IDMFSFEL                        
065000       CALL WMEDKONV USING MED-WMEDAREA                                   
065100       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
065200       PERFORM MFS-RENSA-FAELT-UT                                         
065300                                                                          
065400     END-IF                                                               
065500     .                                                                    
065600     EJECT                                                                
065700 FA-LAES-BEHANDLA-RADINFO SECTION.                                        
065800                                                                          
065900     MOVE +1 TO INDX                                                      
066000     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
066100                   BASEN-SLUT     OR                                      
066200                   INDX > MAX-INDX                                        
066300                                                                          
066400       PERFORM FAA-KOLLA-OM-RAETT-DC                                      
066500                                                                          
066600       IF IDDC-RAETT                                                      
066700                                                                          
066800         PERFORM FAB-LAES-SPARA-KOLLI-INFO                                
066900                                                                          
067000       END-IF                                                             
067100                                                                          
067200       IF EN-KUND                                                         
067300         PERFORM IMS-GN-WDE6A1-DISTR-KVAL                                 
067400       ELSE                                                               
067500         PERFORM IMS-GN-WDE6A1-DISTR                                      
067600       END-IF                                                             
067700     END-PERFORM                                                          
067800                                                                          
067900     PERFORM FAC-KOLLA-OM-FLER-KUNDER                                     
068000     .                                                                    
068100     EJECT                                                                
068200 FAA-KOLLA-OM-RAETT-DC SECTION.                                           
068300                                                                          
068400     IF DCS-IDDC = SEQA-IDDC                                              
068500                                                                          
068600       MOVE JA      TO IDDC-SW                                            
068700                                                                          
068800     ELSE                                                                 
068900       IF W-IDDC-B6 = ZERO                                                
069000                                                                          
069100         MOVE JA        TO IDDC-SW                                        
069200                                                                          
069300       ELSE                                                               
069400         MOVE NEJ       TO IDDC-SW                                        
069500                                                                          
069600       END-IF                                                             
069700     END-IF                                                               
069800     .                                                                    
069900     EJECT                                                                
070000 FAB-LAES-SPARA-KOLLI-INFO SECTION.                                       
070100                                                                          
070200     MOVE SEQA-IDPRODNR TO W-VORD-IDPRODNR                                
070300                          SPAR-IDPRODNR                                   
070400     PERFORM IMS-GU-WDE601-KOLLI                                          
070500                                                                          
070600     IF VORD-FLDIRLEV = 'N'                                               
070700                                                                          
070800       IF (EN-KDORDKL AND VORD-KDORDKL = WS-KDORDKL-NUM) OR               
070900           ALLA-KDORDKL                                                   
071000                                                                          
071100         MOVE VORD-IDKUNDNR   TO SPAR-IDKUNDNR                            
071200         MOVE VORD-IDDC       TO SPAR-IDDC                                
071300         MOVE VORD-KDORDKL    TO SPAR-KDORDKL                             
071400         MOVE VORD-DABEGPAC   TO SPAR-DABEGPAC                            
071500         MOVE VORD-TIPACKN-SK TO SPAR-TIPACKN-SK                          
071600         MOVE VORD-TIFAKT-SK  TO SPAR-TIFAKT-SK                           
071700         MOVE VORD-TILASTN-SK TO SPAR-TILASTN-SK                          
071800                                 WS-TILASTN-SK                            
071900         MOVE VORD-IDPRODNR   TO W-E4-IDPRODNR-MIN                        
071910                                 W-E4-IDPRODNR-MAX                        
071920                                                                          
072000         PERFORM FABB-LAES-VISA-ORDER-INFO                                
072100                                                                          
072200       END-IF                                                             
072300     END-IF                                                               
072400     .                                                                    
072500     EJECT                                                                
072600 FABB-LAES-VISA-ORDER-INFO SECTION.                                       
072700                                                                          
072800     PERFORM IMS-GU-WDE4E                                                 
072900                                                                          
073000     MOVE SEQE-IDORDNR5    TO SPAR-IDORDNR5                               
073100                                                                          
073200     MOVE SEQE-IDDISTR     TO W-SEQC-IDDISTR                              
073300     MOVE SEQE-IDKUNDNR    TO W-SEQC-IDKUNDNR                             
073400                                                                          
073500     MOVE SEQE-IDORDNR5    TO WS-IDORDNR7-NUM                             
073600     MOVE WS-IDORDNR7-NUM  TO W-SEQC-IDKUNDRF                             
073700                                                                          
073800     MOVE SPAR-IDDC        TO W-ARB-IDDC                                  
073900                                                                          
074000     PERFORM IMS-GU-ORQI1-WDQ201-OHUV                                     
074100     IF SEGMENT-FINNS                                                     
074200       MOVE OHUV-TIREGDAT  TO SPAR-TIREGDAT                               
074300       PERFORM FABBA-RAEKNA-FRAM-ARBDAGAR                                 
074400                                                                          
074500       MOVE OHUV-IDORDER   TO W-OHUV-IDORDER                              
074600       PERFORM IMS-GU-ORQI2-WDQ212-ARBTAB                                 
074700                                                                          
074800       IF SEGMENT-FINNS                                                   
074900         MOVE ARB-DATRPAVD TO SPAR-DATRPAVD                               
075000         MOVE ARB-TIHHMM   TO SPAR-TIHHMM                                 
075100                                                                          
075200       ELSE                                                               
075300         MOVE ZERO         TO SPAR-DATRPAVD                               
075400                              SPAR-TIHHMM                                 
075500                              SPAR-TIREGDAT                               
075600       END-IF                                                             
075700     ELSE                                                                 
075800       MOVE ZERO           TO SPAR-DATRPAVD                               
075900                              SPAR-TIHHMM                                 
076000                              SPAR-TIREGDAT                               
076100     END-IF                                                               
076200                                                                          
076300     PERFORM FABBB-FLYTTA-INFO-TILL-MOD                                   
076400     .                                                                    
076500     EJECT                                                                
076600 FABBA-RAEKNA-FRAM-ARBDAGAR SECTION.                                      
076700                                                                          
076800     MOVE +001               TO WORK-KDCALL                               
076810     MOVE DCS-IDDC           TO WORK-IDDC                                 
076900     MOVE OHUV-TIREGDAT      TO WORK-TIAAMMDD-FOM                         
077000     MOVE WS-TILASTN-SK      TO WORK-TIAAMMDD-TOM                         
077100                                                                          
077200     CALL WORKDAY  USING WORK-KDCALL                                      
077300                         WORK-DATE-AREA                                   
077400                         WORK-KDSVAR                                      
077500                                                                          
077600     IF WORK-KDSVAR-OK                                                    
077700       MOVE WORK-KVWORKD     TO SPAR-KVARBDAG                             
077800     ELSE                                                                 
077900       MOVE ZERO             TO SPAR-KVARBDAG                             
078000     END-IF                                                               
078100     .                                                                    
078200     EJECT                                                                
078300 FABBB-FLYTTA-INFO-TILL-MOD SECTION.                                      
078400                                                                          
078500     IF INDX NOT > MAX-INDX                                               
078600                                                                          
078700       MOVE SPAR-IDKUNDNR     TO MOD-IDKUNDNR(INDX)                       
078800       MOVE SPAR-IDORDNR5     TO MOD-IDORDNR7(INDX)                       
078900       MOVE SPAR-IDDC         TO MOD-IDDC(INDX)                           
079000       MOVE SPAR-KDORDKL      TO MOD-KDORDKL(INDX)                        
079100       MOVE SPAR-TIREGDAT     TO MOD-TIREGDAT(INDX)                       
079200       MOVE SPAR-DABEGPAC (3:6) TO MOD-TIBEGPAC(INDX)                     
079300       MOVE SPAR-TIPACKN-SK   TO MOD-TIPACKN-SK(INDX)                     
079400       MOVE SPAR-TIFAKT-SK    TO MOD-TIFAKT-SK(INDX)                      
079500       MOVE SPAR-DATRPAVD (3:6)  TO MOD-TIAAMMDD(INDX)                    
079600       MOVE SPAR-TIHHMM       TO MOD-TIHHMM(INDX)                         
079700       MOVE SPAR-TILASTN-SK   TO MOD-TILASTN-SK(INDX)                     
079800       MOVE SPAR-KVARBDAG     TO MOD-KVARBDAG(INDX)                       
079900                                                                          
080000       IF INDX = 1                                                        
080100         MOVE SPAR-IDKUNDNR   TO MOD-IDKUNDNR-ENTER                       
080200         MOVE SPAR-IDPRODNR   TO MOD-IDPRODNR-ENTER                       
080300         MOVE SPAR-IDORDNR5   TO MOD-IDKUNDRF-ENTER                       
080400       END-IF                                                             
080500                                                                          
080600       ADD +1 TO INDX                                                     
080700                                                                          
080800     END-IF                                                               
080900     .                                                                    
081000     EJECT                                                                
081100 FAC-KOLLA-OM-FLER-KUNDER SECTION.                                        
081200                                                                          
081300     IF SEGMENT-FINNS AND INDX > MAX-INDX                                 
081400                                                                          
081500       MOVE SEQA-IDKUNDNR TO MOD-IDKUNDNR-NEXT                            
081600       MOVE SEQA-IDPRODNR TO MOD-IDPRODNR-NEXT                            
081700                                                                          
081800       MOVE INF-MER-INFO-FINNS TO MED-IDMFSINF                            
081900       CALL WMEDKONV USING MED-WMEDAREA                                   
082000       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
082100                                                                          
082200     END-IF                                                               
082300                                                                          
082400     IF SEGMENT-SAKNAS                                                    
082500                                                                          
082600       MOVE ZERO TO MOD-IDKUNDNR-NEXT                                     
082700                    MOD-IDPRODNR-NEXT                                     
082800                    MOD-IDKUNDRF-NEXT                                     
082900     END-IF                                                               
083000     .                                                                    
083100     EJECT                                                                
083200 S01-INSERT-ALTMSG SECTION.                                               
083300                                                                          
083400     MOVE P-TO-P-SW TO MSG-IO-AREA                                        
083500     PERFORM IMS-CHANGE-ALTMSG                                            
083600     IF STATUS-OK                                                         
083700       PERFORM IMS-INSERT-ALTMSG                                          
083800     ELSE                                                                 
083900       MOVE LOW-VALUE          TO MSG-AREA                                
084000       MOVE 'W4O51701'         TO MFS-IDMOD                               
084100       MOVE '4517'             TO MOD-IDTRANS                             
084200       MOVE P-TO-P-KDTRANS (2:1) TO W-BILD (1:1)                          
084300       MOVE P-TO-P-KDTRANS (4:3) TO W-BILD (2:3)                          
084400       IF SECURITY-FEL                                                    
084500         STRING 'NOT AUTHORIZED TO USE '                                  
084600                W-BILD                                                    
084700                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
084800       ELSE                                                               
084900         STRING 'WRONG PICTURE '                                          
085000                 W-BILD                                                   
085100                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
085200       END-IF                                                             
085300       PERFORM MFS-ROER-EJ-BILD                                           
085400       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O51701 + 4                      
085500       PERFORM IMS-INSERT-MSG                                             
085600     END-IF                                                               
085700     .                                                                    
085800     EJECT                                                                
086000 MFS-RENSA-FAELT-UT SECTION.                                              
086100                                                                          
086200*    --- ALLA UTDATA-FÄLT                                                 
086300     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-ENTER                           
086400                             MOD-IDKUNDNR-NEXT                            
086500                             MOD-IDPRODNR-ENTER                           
086600                             MOD-IDPRODNR-NEXT                            
086700                             MOD-IDKUNDRF-ENTER                           
086800                             MOD-IDKUNDRF-NEXT                            
086900                                                                          
087000     MOVE +1              TO INDX                                         
087100     PERFORM UNTIL INDX > MAX-INDX                                        
087200       MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR(INDX)                         
087300                               MOD-IDORDNR7(INDX)                         
087400                               MOD-IDDC(INDX)                             
087500                               MOD-KDORDKL(INDX)                          
087600                               MOD-TIREGDAT(INDX)                         
087700                               MOD-TIBEGPAC(INDX)                         
087800                               MOD-TIPACKN-SK(INDX)                       
087900                               MOD-TIFAKT-SK(INDX)                        
088000                               MOD-TIAAMMDD(INDX)                         
088100                               MOD-TIHHMM(INDX)                           
088200                               MOD-TILASTN-SK(INDX)                       
088300                               MOD-KVARBDAG(INDX)                         
088400       ADD +1 TO INDX                                                     
088500     END-PERFORM                                                          
088600     .                                                                    
088700     EJECT                                                                
088800 MFS-ROER-EJ-BILD   SECTION.                                              
088900                                                                          
089000*    --- ALLA UTDATA-FÄLT                                                 
089100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR-ENTER                         
089200                               MOD-IDKUNDNR-NEXT                          
089300                               MOD-IDPRODNR-ENTER                         
089400                               MOD-IDPRODNR-NEXT                          
089500                               MOD-IDKUNDRF-ENTER                         
089600                               MOD-IDKUNDRF-NEXT                          
089700                                                                          
089800     MOVE +1              TO INDX                                         
089900     PERFORM UNTIL INDX > MAX-INDX                                        
090000       MOVE MFS-ROER-EJ-FAELT TO MOD-IDKUNDNR(INDX)                       
090100                                 MOD-IDORDNR7(INDX)                       
090200                                 MOD-IDDC(INDX)                           
090300                                 MOD-KDORDKL(INDX)                        
090400                                 MOD-TIREGDAT(INDX)                       
090500                                 MOD-TIBEGPAC(INDX)                       
090600                                 MOD-TIPACKN-SK(INDX)                     
090700                                 MOD-TIFAKT-SK(INDX)                      
090800                                 MOD-TIAAMMDD(INDX)                       
090900                                 MOD-TIHHMM(INDX)                         
091000                                 MOD-TILASTN-SK(INDX)                     
091100                                 MOD-KVARBDAG(INDX)                       
091200       ADD +1 TO INDX                                                     
091300     END-PERFORM                                                          
091400     .                                                                    
091500     EJECT                                                                
091600* --- IMS SEKTIONER ---                                                   
091700     SKIP3                                                                
091800 IMS-GET-MSG SECTION.                                                     
091900                                                                          
092000     MOVE '  QC' TO GODK-STATUSKODER                                      
092100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
092200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
092300     PERFORM IMS-STATUSKONTROLL                                           
092400     .                                                                    
092500     SKIP3                                                                
092600 IMS-INSERT-MSG SECTION.                                                  
092700                                                                          
092800     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
092900       MOVE '0' TO MFS-KDHUVOMR                                           
093000     END-IF                                                               
093100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
093200     MOVE SPACE TO GODK-STATUSKODER                                       
093300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
093400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
093500     PERFORM IMS-STATUSKONTROLL                                           
093600     .                                                                    
093700     EJECT                                                                
093800 IMS-CHANGE-ALTMSG SECTION.                                               
093900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
094000     MOVE '  A1A4' TO GODK-STATUSKODER                                    
094100     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
094200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
094300     PERFORM IMS-STATUSKONTROLL                                           
094400     .                                                                    
094500     SKIP3                                                                
094600 IMS-INSERT-ALTMSG SECTION.                                               
094700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
094800     MOVE SPACE TO GODK-STATUSKODER                                       
094900     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
095000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
095100     PERFORM IMS-STATUSKONTROLL                                           
095200     .                                                                    
095300     EJECT                                                                
095400 IMS-GU-WDE6A1-DISTR-UNIK SECTION.                                        
095500                                                                          
095600     STRING 'WDE6A1  (WDE6A1KY>=' W-WDE6A1KY-MIN-X                        
095700                    '&WDE6A1KY<=' W-WDE6A1KY-MAX-X                        
095800                    '&KDORDSTA =' W-SEK-KDORDSTA-X                        
095900                    '&IDKUNDNR =' W-SEK-IDKUNDNR-X                        
096000                    '&IDPRODNR =' W-SEK-IDPRODNR-X ')'                    
096100          DELIMITED BY SIZE INTO SSA1                                     
096200     MOVE '  GE' TO GODK-STATUSKODER                                      
096300     CALL CBLTDLI USING GU WDE6A-PCB DLI-IO-WDE6A1 SSA1                   
096400     MOVE WDE6A-STATUS-CODE TO STATUS-WS                                  
096500     PERFORM IMS-STATUSKONTROLL                                           
096600     .                                                                    
096700     SKIP3                                                                
096800 IMS-GU-WDE6A1-DISTR-KUND SECTION.                                        
096900                                                                          
097000     STRING 'WDE6A1  (WDE6A1KY>=' W-WDE6A1KY-MIN-X                        
097100                    '&WDE6A1KY<=' W-WDE6A1KY-MAX-X                        
097200                    '&KDORDSTA =' W-SEK-KDORDSTA-X                        
097300                    '&IDKUNDNR =' W-SEK-IDKUNDNR-X ')'                    
097400          DELIMITED BY SIZE INTO SSA1                                     
097500     MOVE '  GE' TO GODK-STATUSKODER                                      
097600     CALL CBLTDLI USING GU WDE6A-PCB DLI-IO-WDE6A1 SSA1                   
097700     MOVE WDE6A-STATUS-CODE TO STATUS-WS                                  
097800     PERFORM IMS-STATUSKONTROLL                                           
097900     .                                                                    
098000     EJECT                                                                
098100 IMS-GU-WDE6A1-DISTR-OKVAL SECTION.                                       
098200                                                                          
098300     STRING 'WDE6A1  (WDE6A1KY>=' W-WDE6A1KY-MIN-X                        
098400                    '&WDE6A1KY<=' W-WDE6A1KY-MAX-X                        
098500                    '&KDORDSTA =' W-SEK-KDORDSTA-X ')'                    
098600          DELIMITED BY SIZE INTO SSA1                                     
098700     MOVE '  GE' TO GODK-STATUSKODER                                      
098800     CALL CBLTDLI USING GU WDE6A-PCB DLI-IO-WDE6A1 SSA1                   
098900     MOVE WDE6A-STATUS-CODE TO STATUS-WS                                  
099000     PERFORM IMS-STATUSKONTROLL                                           
099100     .                                                                    
099200     SKIP3                                                                
099300 IMS-GN-WDE6A1-DISTR SECTION.                                             
099400                                                                          
099500     STRING 'WDE6A1  (WDE6A1KY>=' W-WDE6A1KY-MIN-X                        
099600                    '&WDE6A1KY<=' W-WDE6A1KY-MAX-X                        
099700                    '&KDORDSTA =' W-SEK-KDORDSTA-X ')'                    
099800          DELIMITED BY SIZE INTO SSA1                                     
099900     MOVE '  GBGE' TO GODK-STATUSKODER                                    
100000     CALL CBLTDLI USING GN WDE6A-PCB DLI-IO-WDE6A1 SSA1                   
100100     MOVE WDE6A-STATUS-CODE TO STATUS-WS                                  
100200     PERFORM IMS-STATUSKONTROLL                                           
100300     .                                                                    
100400     EJECT                                                                
100500 IMS-GN-WDE6A1-DISTR-KVAL SECTION.                                        
100600                                                                          
100700     STRING 'WDE6A1  (WDE6A1KY>=' W-WDE6A1KY-MIN-X                        
100800                    '&WDE6A1KY<=' W-WDE6A1KY-MAX-X                        
100900                    '&KDORDSTA =' W-SEK-KDORDSTA-X                        
101000                    '&IDKUNDNR =' W-SEK-IDKUNDNR-X ')'                    
101100          DELIMITED BY SIZE INTO SSA1                                     
101200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
101300     CALL CBLTDLI USING GN WDE6A-PCB DLI-IO-WDE6A1 SSA1                   
101400     MOVE WDE6A-STATUS-CODE TO STATUS-WS                                  
101500     PERFORM IMS-STATUSKONTROLL                                           
101600     .                                                                    
101700     SKIP3                                                                
101800 IMS-GU-WDE601-KOLLI SECTION.                                             
101900                                                                          
102000     STRING 'WDE601  (IDPRODNR =' W-WDE601-IDPRODNR-X ')'                 
102100          DELIMITED BY SIZE INTO SSA1                                     
102200     MOVE '  ' TO GODK-STATUSKODER                                        
102300     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
102400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
102500     PERFORM IMS-STATUSKONTROLL                                           
102600     .                                                                    
102700     EJECT                                                                
102800 IMS-GU-WDE4E SECTION.                                                    
102900                                                                          
102910     STRING 'WDE4E1  (WDE4E1KY>=' W-WDE4E1KY-MIN                          
102911                    '&WDE4E1KY<=' W-WDE4E1KY-MAX ')'                      
102920          DELIMITED BY SIZE INTO SSA1                                     
103100     MOVE '  ' TO GODK-STATUSKODER                                        
103200     CALL CBLTDLI USING GU WDE4E-PCB DLI-IO-WDE4E1 SSA1                   
103300     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
103400     PERFORM IMS-STATUSKONTROLL                                           
103500     .                                                                    
103600     SKIP3                                                                
103700 IMS-GU-ORQI1-WDQ201-OHUV SECTION.                                        
103800                                                                          
103900     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
104000          DELIMITED BY SIZE INTO SSA1                                     
104100     MOVE '  GE' TO GODK-STATUSKODER                                      
104200     CALL CBLTDLI USING GU ORQI1-PCB DLI-IO-AREA SSA1                     
104300     MOVE ORQI1-STATUS-CODE TO STATUS-WS                                  
104400     PERFORM IMS-STATUSKONTROLL                                           
104500     .                                                                    
104600     EJECT                                                                
104700 IMS-GU-ORQI2-WDQ212-ARBTAB SECTION.                                      
104800                                                                          
104900     STRING 'WLORQI01(IDORDER  =' W-WDQ201-IDORDER-X ')'                  
105000          DELIMITED BY SIZE INTO SSA1                                     
105100     STRING 'WLORQI12(IDDC     =' W-WDQ212-IDDC-X ')'                     
105200          DELIMITED BY SIZE INTO SSA2                                     
105300     MOVE '  GE' TO GODK-STATUSKODER                                      
105400     CALL CBLTDLI USING GU ORQI2-PCB DLI-IO-AREA SSA1 SSA2                
105500     MOVE ORQI2-STATUS-CODE TO STATUS-WS                                  
105600     PERFORM IMS-STATUSKONTROLL                                           
105700     .                                                                    
105800     SKIP3                                                                
105810 IMS-GU-WDB601    SECTION.                                                
105820     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
105830          DELIMITED BY SIZE INTO SSA1                                     
105840     MOVE '  GE' TO GODK-STATUSKODER                                      
105850     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
105860     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
105870     PERFORM IMS-STATUSKONTROLL                                           
105880     IF SEGMENT-SAKNAS                                                    
105890         MOVE SPACE TO DCS-KDDC                                           
105891     END-IF                                                               
105892     .                                                                    
105900 IMS-STATUSKONTROLL SECTION.                                              
106000                                                                          
106100     SET STATUS-IX TO 1                                                   
106200     SEARCH GODK-STATUS                                                   
106300       AT END CALL FELLOG                                                 
106400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
106500     END-SEARCH                                                           
106600     .                                                                    
