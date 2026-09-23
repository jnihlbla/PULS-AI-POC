000120 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0050800.                                                
000300 AUTHOR.         RICHARD.                                                 
000500 DATE-WRITTEN.   JAN.  93.                                                
000510 DATE-COMPILED.                                                           
000520                                                                          
000540*    FUNKTION:                                                            
000550*        TEST-PROGRAM FÖR STANDARDSUB-PROGRAM.                            
000560*             SUBPROGRAM W006PRT                                          
000570*                       (PRINTER OCH NODE)                                
000620*                    OCH W006PRC1                                         
000630*                       (PRINTNING GENOM VCOM OCH SPOOL-API)              
000640*                    OCH W006PRR1                                         
000650*                       (PRINTNING MED ÅTERSTART OCH SPOOL-API)           
000660*                    OCH W006PRS1                                         
000670*                       (PRINTNING UTAN ÅTERSTART MED SPOOL-API)          
000680*                    OCH W005INIT                                         
000690*                       (USER-DB TEST)                                    
000900                                                                          
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 DATA DIVISION.                                                           
001300     EJECT                                                                
001450 WORKING-STORAGE SECTION.                                                 
001451                                                                          
001460*    -- CHECKED BY WY2000                                                 
001500 77    IDPGM                 PIC X(8)    VALUE 'W0050800'.                
001510 77    W-COMPILED            PIC X(16)   VALUE SPACE.                     
001520 77    FELTEXT               PIC X(80)   VALUE SPACE.                     
001600 77    JA                    PIC X       VALUE 'J'.                       
001700 77    NEJ                   PIC X       VALUE 'N'.                       
001710 77    INDX                  PIC S9(9)   VALUE ZERO     COMP SYNC.        
001720 77    W-DATE                PIC 9(6)    VALUE ZERO.                      
001730 77    W-TIME                PIC 9(8)    VALUE ZERO.                      
001800 77    W-LISTNR-1            PIC X(10)   VALUE 'LISTA-1   '.              
001801 77    W-LISTNR-2            PIC X(10)   VALUE 'LISTA-2   '.              
001802 77    W-DUMMY               PIC X(8)    VALUE SPACE.                     
001803 77    TEST-36000-2          PIC X(8)    VALUE '002     '.                
001804 77    TEST-SPOOL-2          PIC X(8)    VALUE '00E     '.                
001810                                                                          
001811 01    STYR-POST.                                                         
001812   03    STYR-IDPGM          PIC X(8).                                    
001813   03    STYR-IDPGM-UT       PIC X(8).                                    
001814   03    STYR-INDATA         PIC X(50).                                   
001815   03    STYR-INDATA-UT      PIC X(50).                                   
001817                                                                          
001818 01    INIT-POST.                                                         
001819   03    IN-KDCALL           PIC X(3).                                    
001820   03    IN-IDUSER           PIC X(8).                                    
001830   03    IN-IDLTERM          PIC X(8).                                    
001840   03    IN-TILOKDAT         PIC X(6).                                    
001841   03    IN-TILOKTID         PIC X(8).                                    
001850                                                                          
001871     SKIP3                                                                
001872 01    DYNAMISKA-SUBPROGRAM.                                              
001873   03    W006PRT             PIC X(8)    VALUE 'W006PRT '.                
001876   03    W006PRC1            PIC X(8)    VALUE 'W006PRC1'.                
001877   03    W006PRR1            PIC X(8)    VALUE 'W006PRR1'.                
001878   03    W006PRS1            PIC X(8)    VALUE 'W006PRS1'.                
001880   03    W005INIT            PIC X(8)    VALUE 'W005INIT'.                
001897   03    CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
001900   03    FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
002200     EJECT                                                                
002210 01    PRT-AREA-START        PIC X(16)   VALUE 'PRT-START'.               
002220*01    -COPY W006PRT                                                      
002221     EJECT                                                                
002222 01    WMSGINIT-START        PIC X(16)   VALUE 'WMSGINIT-START'.          
002223*01    -COPY WMSGINIT                                                     
002230     EJECT                                                                
002240 01    SIDA.                                                              
002250   03    RAD1.                                                            
002251     05    FILLER            PIC X(10)   VALUE 'RAD 001   '.              
002252     05    RAD-IDPGM         PIC X(10)   VALUE SPACE.                     
002253     05    FILLER            PIC X(14)   VALUE '        30    '.          
002254     05    RAD-DATUM         PIC 9(6)    VALUE ZERO.                      
002255     05    FILLER            PIC X(11)   VALUE '        50 '.             
002256     05    RAD-TIME          PIC 9(9)    VALUE ZERO.                      
002257     05    FILLER            PIC X(10)   VALUE '        70'.              
002258     05    RAD-LISTNR        PIC X(10)   VALUE SPACE.                     
002259     05    FILLER            PIC X(10)   VALUE '        90'.              
002260     05    FILLER            PIC X(10)   VALUE '       100'.              
002261     05    FILLER            PIC X(10)   VALUE '       110'.              
002262     05    FILLER            PIC X(10)   VALUE '       120'.              
002263     05    FILLER            PIC X(12)   VALUE '         132'.            
002290   03    RAD2.                                                            
002291     05    FILLER            PIC X(4)    VALUE 'RAD '.                    
002292     05    RAD               PIC 9(3)    VALUE 001.                       
002293     05    FILLER            PIC X(40)   VALUE SPACE.                     
002294     05    FILLER            PIC X(3)    VALUE '50A'.                     
002295     05    FILLER            PIC X(27)   VALUE SPACE.                     
002296     05    FILLER            PIC X(3)    VALUE '80A'.                     
002297     05    FILLER            PIC X(33)   VALUE SPACE.                     
002298     05    FILLER            PIC X(3)    VALUE '116'.                     
002299     05    FILLER            PIC X(13)   VALUE SPACE.                     
002300     05    FILLER            PIC X(3)    VALUE '132'.                     
002301   03    SLUTRAD             PIC X(132)  VALUE 'SLUTRAD                   
002302-      '20        30        40        50        60        70              
002303-      '  80        90       100       110       120         132'.        
002305     EJECT                                                                
002306*01    -COPY W006PRAR                                                     
002307     EJECT                                                                
002310******************************************************************        
002400*                                                                         
002500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
002600*                                                                         
002700 01    FILLER                PIC X(16)   VALUE 'MFS-WS          '.        
002800     SKIP3                                                                
002900*01    -COPY WMSGAREA                                                     
003000   03    MOD-W0O50801            REDEFINES MSG-AREA.                      
003100     05    MOD-IDTRANS       PIC X(4).                                    
003200     05    MOD-TEMFSFEL      PIC X(40).                                   
003201     05    MOD-IDPGM-IN      PIC X(8).                                    
003202     05    MOD-IDPGM-UT      PIC X(8).                                    
003203     05    MOD-INDATA-IN     PIC X(50).                                   
003204     05    MOD-INDATA-UT     PIC X(50).                                   
003210     05    MOD-RADER.                                                     
003211       07  MOD-RAD OCCURS 15 PIC X(79).                                   
003220     05    MOD-TEMFSINF      PIC X(55).                                   
003300     EJECT                                                                
003400*01    -COPY WMFSAREA                                                     
003500     EJECT                                                                
003600******************************************************************        
003700*                                                                         
003800*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
003900*                                                                         
004000 01    IMS-WS.                                                            
004100   03    FILLER              PIC X(16)   VALUE 'IMS-WS     '.             
004200     SKIP3                                                                
004300   03    STATUS-WS           PIC XX.                                      
004400     88    SEGMENT-FINNS                 VALUE '  '.                      
004500     88    SEGMENT-SAKNAS                VALUE 'GE'.                      
004600     SKIP3                                                                
004700   03    GODK-STATUSKODER.                                                
004800     05    GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.              
004900     EJECT                                                                
005000*01    -COPY W0003                                                        
005100     EJECT                                                                
005110 LINKAGE SECTION.                                                         
005120*01    -COPY W0009     -PRE MSG-                                          
005130*01    -COPY W0009     -PRE ALT-                                          
005140     EJECT                                                                
005150*01    -COPY W0008     -PRE LISB-                                         
005160     05  FILLER              PIC X(1).                                    
005170                                                                          
005180*01    -COPY W0008     -PRE USEA-                                         
005190     05  FILLER              PIC X(1).                                    
005400     EJECT                                                                
005500 PROCEDURE DIVISION USING MSG-PCB ALT-PCB LISB-PCB USEA-PCB.              
005600 MAIN SECTION.                                                            
005610     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB LISB-PCB USEA-PCB.             
005700                                                                          
005800     PERFORM IMS-GET-MSG                                                  
005900     IF SEGMENT-FINNS                                                     
006000       PERFORM A-INIT-SPARA-INPUT                                         
006050       EVALUATE STYR-IDPGM                                                
006051         WHEN 'W006PRT '                                                  
006060           PERFORM B-W006PRT                                              
006100         WHEN 'W006PRC1'                                                  
006101           PERFORM E-W006PRC1                                             
006103         WHEN 'W006PRS1'                                                  
006104           PERFORM F-W006PRS1                                             
006131         WHEN 'W006PRR1'                                                  
006132           PERFORM H-W006PRR1                                             
006137         WHEN 'W005INIT'                                                  
006141             PERFORM M-W005INIT                                           
006152         WHEN 'ABEND   '                                                  
006153           CALL FELLOG                                                    
006157         WHEN OTHER                                                       
006158           MOVE ' FEL STYR-PARAMETER' TO MOD-TEMFSFEL                     
006159       END-EVALUATE                                                       
006192                                                                          
006900       COMPUTE MSG-KVLL = LENGTH OF MOD-W0O50801 + 4                      
007000       PERFORM IMS-INSERT-MSG                                             
007100     END-IF                                                               
007110                                                                          
007200     MOVE ZERO TO RETURN-CODE                                             
007300     GOBACK                                                               
007400     .                                                                    
007500     EJECT                                                                
007600 A-INIT-SPARA-INPUT SECTION.                                              
007700     IF MSG-DUBBLA-TRANSKODER                                             
007710       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO STYR-POST                    
007800       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
007900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
008000     ELSE                                                                 
008010       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO STYR-POST                      
008100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
008200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
008300     END-IF                                                               
008301     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
008302     MOVE MSG-IDPFK TO MFS-IDPFK                                          
008310     MOVE MFS-IDTRANS TO MOD-IDTRANS                                      
008320     MOVE ' TEST      KLAR ' TO MOD-TEMFSFEL                              
008330     MOVE 'W0O50801' TO MFS-IDMOD                                         
008340                                                                          
008350     IF STYR-IDPGM = ALL '+'                                              
008360       MOVE STYR-IDPGM-UT TO STYR-IDPGM                                   
008370     END-IF                                                               
008380     MOVE STYR-IDPGM TO MOD-IDPGM-UT                                      
008381     IF STYR-INDATA = ALL '+'                                             
008382       MOVE STYR-INDATA-UT TO STYR-INDATA                                 
008384     END-IF                                                               
008385     MOVE STYR-INDATA TO MOD-INDATA-UT                                    
008391     MOVE MFS-RENSA-FAELT TO MOD-IDPGM-IN MOD-INDATA-IN                   
008392                                                                          
008393     MOVE STYR-IDPGM TO RAD-IDPGM                                         
008394     ACCEPT W-DATE FROM DATE                                              
008395     MOVE W-DATE TO RAD-DATUM                                             
008396     ACCEPT W-TIME FROM TIME                                              
008397     MOVE W-TIME TO RAD-TIME                                              
008400     .                                                                    
014900     EJECT                                                                
027100 B-W006PRT SECTION.                                                       
027200                                                                          
027210     MOVE +1 TO INDX                                                      
027220                                                                          
027300     MOVE 001 TO PRT-KDCALL                                               
027400     MOVE STYR-INDATA TO PRT-IDPRTLST                                     
027500     CALL W006PRT USING PRT-W006PRT                                       
027600     PERFORM BA-W006PRT                                                   
027700                                                                          
027701     MOVE 001 TO PRT-KDCALL                                               
027702     MOVE STYR-INDATA-UT TO PRT-IDPRTLST                                  
027703     CALL W006PRT USING PRT-W006PRT                                       
027704     PERFORM BA-W006PRT                                                   
027705                                                                          
027710     MOVE 001 TO PRT-KDCALL                                               
027720     MOVE '12345678' TO PRT-IDPRTLST                                      
027730     CALL W006PRT USING PRT-W006PRT                                       
027740     PERFORM BA-W006PRT                                                   
027750                                                                          
027800     MOVE 001 TO PRT-KDCALL                                               
027900     MOVE 'SPA     ' TO PRT-IDPRTLST                                      
028000     CALL W006PRT USING PRT-W006PRT                                       
028100     PERFORM BA-W006PRT                                                   
028200                                                                          
028300     MOVE 001 TO PRT-KDCALL                                               
028400     MOVE '461     ' TO PRT-IDPRTLST                                      
028500     CALL W006PRT USING PRT-W006PRT                                       
028600     PERFORM BA-W006PRT                                                   
028700                                                                          
028800     MOVE 002 TO PRT-KDCALL                                               
028900     MOVE 'W1234567' TO PRT-IDLTERM                                       
029000     CALL W006PRT USING PRT-W006PRT                                       
029100     PERFORM BA-W006PRT                                                   
029200                                                                          
029300     MOVE 002 TO PRT-KDCALL                                               
029400     MOVE 'R17850  ' TO PRT-IDLTERM                                       
029500     CALL W006PRT USING PRT-W006PRT                                       
029600     PERFORM BA-W006PRT                                                   
029700                                                                          
029800     MOVE 003 TO PRT-KDCALL                                               
029900     MOVE 'AAAAAAAA' TO PRT-IDPRTLST                                      
030000     CALL W006PRT USING PRT-W006PRT                                       
030100     PERFORM BA-W006PRT                                                   
030200                                                                          
030300     MOVE 003 TO PRT-KDCALL                                               
030400     MOVE '001     ' TO PRT-IDPRTLST                                      
030500     CALL W006PRT USING PRT-W006PRT                                       
030600     PERFORM BA-W006PRT                                                   
030610                                                                          
030620     MOVE 004 TO PRT-KDCALL                                               
030630     MOVE 'R17850  ' TO PRT-IDNODE                                        
030640     CALL W006PRT USING PRT-W006PRT                                       
030650     PERFORM BA-W006PRT                                                   
030651                                                                          
030652     MOVE 004 TO PRT-KDCALL                                               
030653     MOVE STYR-INDATA TO PRT-IDNODE                                       
030655     CALL W006PRT USING PRT-W006PRT                                       
030656     PERFORM BA-W006PRT                                                   
030660                                                                          
030670     MOVE 004 TO PRT-KDCALL                                               
030671     MOVE STYR-INDATA-UT TO PRT-IDNODE                                    
030690     CALL W006PRT USING PRT-W006PRT                                       
030691     PERFORM BA-W006PRT                                                   
030700     .                                                                    
030800     EJECT                                                                
030900 BA-W006PRT SECTION.                                                      
031000                                                                          
031100     STRING  ' ' PRT-KDCALL   ' ' PRT-IDPRTLST                            
031200         ' ' PRT-IDLTERM  ' ' PRT-IDNODE                                  
031300         ' ' PRT-BEPRTLST ' ' PRT-KDSVAR                                  
031310          DELIMITED BY SIZE INTO MOD-RAD (INDX)                           
031400     ADD +1 TO INDX                                                       
031410     .                                                                    
042500     EJECT                                                                
042600 E-W006PRC1 SECTION.                                                      
042700                                                                          
043000     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-OPEN  TEST-SPOOL-2             
043100                         ALT-PCB                                          
043200                         W-DUMMY W-DUMMY                                  
043300                                                                          
043400     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-SPOOL-2             
043500                         ALT-PCB                                          
043600                         PRT-NYSIDA-RAD1 RAD1                             
043700                                                                          
043800     MOVE 1 TO RAD                                                        
043900     PERFORM 69 TIMES                                                     
044000       ADD 1 TO RAD                                                       
044100       CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-SPOOL-2           
044200                           ALT-PCB                                        
044300                           PRT-AFTER-1 RAD2                               
044400     END-PERFORM                                                          
044500                                                                          
045200     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-OPEN  TEST-SPOOL-2             
045300                         ALT-PCB                                          
045400                         W-DUMMY W-DUMMY                                  
045500                                                                          
045600     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-SPOOL-2             
045700                         ALT-PCB                                          
045800                         PRT-NYSIDA-RAD1 RAD1                             
045900                                                                          
046000     MOVE 1 TO RAD                                                        
046100     PERFORM 40 TIMES                                                     
046200       ADD 1 TO RAD                                                       
046300       CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-SPOOL-2           
046400                           ALT-PCB                                        
046500                           PRT-AFTER-1 RAD2                               
046600     END-PERFORM                                                          
046700                                                                          
047200     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-SPOOL-2             
047300                         ALT-PCB                                          
047400                         PRT-NYSIDA-RAD1 RAD1                             
047500                                                                          
047600     MOVE 1 TO RAD                                                        
047700     PERFORM 47 TIMES                                                     
047800       ADD 1 TO RAD                                                       
047900       CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-SPOOL-2           
048000                           ALT-PCB                                        
048100                           PRT-AFTER-1 RAD2                               
048200     END-PERFORM                                                          
048300                                                                          
048400     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-WRITE TEST-SPOOL-2             
048500                         ALT-PCB                                          
048600                         PRT-AFTER-1 SLUTRAD                              
048700                                                                          
048800     CALL W006PRC1 USING PRT-SPOOL-OVR PRT-CLOSE TEST-SPOOL-2             
048900                         ALT-PCB                                          
049000                         W-DUMMY W-DUMMY                                  
049100     .                                                                    
049200     EJECT                                                                
049300 F-W006PRS1 SECTION.                                                      
049400                                                                          
049700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-OPEN TEST-36000-2              
049800                         ALT-PCB                                          
049900                         W-DUMMY W-DUMMY                                  
050000                                                                          
050100     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE TEST-36000-2             
050200                         ALT-PCB                                          
050300                         PRT-NYSIDA-RAD1 RAD1                             
050400                                                                          
050500     MOVE 1 TO RAD                                                        
050600     PERFORM 69 TIMES                                                     
050700       ADD 1 TO RAD                                                       
050800       CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE TEST-36000-2           
050900                           ALT-PCB                                        
051000                           PRT-AFTER-1 RAD2                               
051100     END-PERFORM                                                          
051200                                                                          
051700     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE TEST-36000-2             
051800                         ALT-PCB                                          
051900                         PRT-NYSIDA-RAD1 RAD1                             
052000                                                                          
052100     MOVE 1 TO RAD                                                        
052200     PERFORM 69 TIMES                                                     
052300       ADD 1 TO RAD                                                       
052400       CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE TEST-36000-2           
052500                           ALT-PCB                                        
052600                           PRT-AFTER-1 RAD2                               
052700     END-PERFORM                                                          
052800                                                                          
053300     CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE TEST-36000-2             
053400                         ALT-PCB                                          
053500                         PRT-NYSIDA-RAD1 RAD1                             
053600                                                                          
053700     MOVE 1 TO RAD                                                        
053800     PERFORM 47 TIMES                                                     
053900       ADD 1 TO RAD                                                       
054000       CALL W006PRS1 USING PRT-SPOOL-A4S PRT-WRITE TEST-36000-2           
054100                           ALT-PCB                                        
054200                           PRT-AFTER-1 RAD2                               
054300     END-PERFORM                                                          
054400                                                                          
054900     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
055000                         ALT-PCB                                          
055100                         PRT-NYSIDA-RAD1 RAD1                             
055200                                                                          
055300     MOVE 1 TO RAD                                                        
055400     PERFORM 47 TIMES                                                     
055500       ADD 1 TO RAD                                                       
055600       CALL W006PRS1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
055700                           ALT-PCB                                        
055800                           PRT-AFTER-1 RAD2                               
055900     END-PERFORM                                                          
056000                                                                          
056500     CALL W006PRS1 USING PRT-SPOOL-OVR PRT-CLOSE TEST-36000-2             
056600                         ALT-PCB                                          
056700                         W-DUMMY W-DUMMY                                  
056800                                                                          
057100     .                                                                    
062100     EJECT                                                                
062110 H-W006PRR1 SECTION.                                                      
062120                                                                          
062121     CALL W006PRR1 USING PRT-SPOOL-OVR PRT-OPEN TEST-SPOOL-2              
062122                         ALT-PCB LISB-PCB W-DUMMY                         
062123                         W-DUMMY W-DUMMY                                  
062124                                                                          
062125     CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE TEST-SPOOL-2             
062126                         ALT-PCB LISB-PCB W-LISTNR-1                      
062127                         PRT-NYSIDA-RAD1 RAD1                             
062128                                                                          
062129     MOVE 1 TO RAD                                                        
062130     PERFORM 47 TIMES                                                     
062131       ADD 1 TO RAD                                                       
062132       CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE TEST-SPOOL-2           
062133                           ALT-PCB LISB-PCB W-LISTNR-1                    
062134                           PRT-AFTER-1 RAD2                               
062135     END-PERFORM                                                          
062136                                                                          
062137     CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2             
062138                         ALT-PCB LISB-PCB W-LISTNR-2                      
062139                         PRT-NYSIDA-RAD1 RAD1                             
062140                                                                          
062141     MOVE 1 TO RAD                                                        
062142     PERFORM 29 TIMES                                                     
062143       ADD 1 TO RAD                                                       
062144       CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
062145                           ALT-PCB LISB-PCB W-LISTNR-2                    
062146                           PRT-AFTER-1 RAD2                               
062147     END-PERFORM                                                          
062148                                                                          
062149     PERFORM  4 TIMES                                                     
062150                                                                          
062151       CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2           
062152                           ALT-PCB LISB-PCB W-LISTNR-2                    
062153                           PRT-NYSIDA-RAD1 RAD1                           
062154                                                                          
062155       MOVE 1 TO RAD                                                      
062156       PERFORM 10 TIMES                                                   
062157         ADD 2 TO RAD                                                     
062158         CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2         
062159                             ALT-PCB LISB-PCB W-LISTNR-2                  
062160                             PRT-AFTER-2 RAD2                             
062161         ADD 1 TO RAD                                                     
062162         CALL W006PRR1 USING PRT-SPOOL-OVR PRT-WRITE TEST-36000-2         
062163                             ALT-PCB LISB-PCB W-LISTNR-2                  
062164                             PRT-AFTER-1 RAD2                             
062165       END-PERFORM                                                        
062166                                                                          
062167     END-PERFORM                                                          
062168                                                                          
062169     CALL W006PRR1 USING PRT-SPOOL-OVR PRT-CLOSE TEST-36000-2             
062170                         ALT-PCB LISB-PCB W-DUMMY                         
062171                         W-DUMMY W-DUMMY                                  
062172     .                                                                    
062173     EJECT                                                                
062174 M-W005INIT SECTION.                                                      
062175                                                                          
062176     MOVE STYR-INDATA TO INIT-POST                                        
062177     IF IN-KDCALL = '001' OR '002' OR '003' OR '011' OR '012'             
062178       MOVE IN-KDCALL          TO MSGI-KDCALL                             
062179     ELSE                                                                 
062180       MOVE '001'              TO MSGI-KDCALL                             
062182     END-IF                                                               
062183     IF IN-IDUSER = '++++++++' OR SPACE                                   
062184       MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                             
062185     ELSE                                                                 
062186       MOVE IN-IDUSER          TO MSGI-IDUSER                             
062187     END-IF                                                               
062188     IF IN-IDLTERM = '++++++++' OR SPACE                                  
062189       MOVE MSGI-IDUSER        TO MSGI-IDLTERM-USER                       
062190     ELSE                                                                 
062191       MOVE IN-IDLTERM         TO MSGI-IDLTERM-USER                       
062192     END-IF                                                               
062193     IF IN-TILOKDAT = '++++++' OR SPACE                                   
062194       MOVE W-DATE             TO IN-TILOKDAT                             
062195     END-IF                                                               
062196     MOVE IN-TILOKDAT          TO MSGI-TILOKDAT                           
062198     IF IN-TILOKTID = '++++++++' OR SPACE                                 
062199       MOVE W-TIME             TO IN-TILOKTID                             
062200     END-IF                                                               
062202     MOVE IN-TILOKTID          TO MSGI-TILOKTID                           
062204     MOVE 'TEST'               TO MSGI-IDTRANS                            
062205     MOVE '1'                  TO MSGI-KDMFSFOR                           
062209     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
062210     PERFORM LA-W005INIT                                                  
062211     .                                                                    
062212     EJECT                                                                
062213 LA-W005INIT SECTION.                                                     
062214                                                                          
062215     MOVE MSGI-WMSGINIT TO MOD-RADER                                      
062216     .                                                                    
062217     EJECT                                                                
062218* IMS SEKTIONER                                                           
062219     SKIP3                                                                
062220 IMS-GET-MSG SECTION.                                                     
062221     MOVE '  QC' TO GODK-STATUSKODER                                      
062222     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
062223     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062224     PERFORM IMS-STATUSKONTROLL                                           
062225     .                                                                    
062226     SKIP3                                                                
062227 IMS-INSERT-MSG SECTION.                                                  
062228     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
062229     MOVE SPACE TO GODK-STATUSKODER                                       
062230     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
062231     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062232     PERFORM IMS-STATUSKONTROLL                                           
062240     .                                                                    
062300     EJECT                                                                
064600 IMS-STATUSKONTROLL SECTION.                                              
064700                                                                          
064800     SET STATUS-IX TO 1                                                   
064900     SEARCH GODK-STATUS                                                   
065000       AT END                                                             
065100         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT                         
065110         CALL FELLOG                                                      
065200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
065300         CONTINUE                                                         
065400     END-SEARCH                                                           
065500     .                                                                    
