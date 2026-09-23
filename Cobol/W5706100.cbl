000101 ID DIVISION.                                                             
000201 PROGRAM-ID.     W5706100.                                                
000301 AUTHOR.         ANDERS HENRIKSSON.                                       
000401 DATE-WRITTEN.   20120103.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNKTION:                                                            
000801*        PROGRAMMET LÄSER                                                 
000901*        1 EKONOMISK HÄNDELSEFIL                                          
001001*                                                                         
001101*        OCH SKAPAR                                                       
001201*        2 FIL TILL KONTROLL                                              
001401*                                                                         
001501     EJECT                                                                
001601                                                                          
001701 ENVIRONMENT DIVISION.                                                    
001801                                                                          
001901 INPUT-OUTPUT SECTION.                                                    
002001                                                                          
002101 FILE-CONTROL.                                                            
002201     SKIP2                                                                
002301*          --- INFIL EKONOMI                                              
002401     SELECT W57061                     ASSIGN TO W57061D1.                
002501     SKIP2                                                                
002502*          --- INFIL EKONOMI                                              
002503     SELECT W5706A                     ASSIGN TO W57061D2.                
002504     SKIP2                                                                
002601*          --- UTFIL KONTROLL                                             
002701     SELECT W57062                     ASSIGN TO W57061D3.                
002801     SKIP2                                                                
002802*          --- UTFIL KONTROLL                                             
002803     SELECT W5706B                     ASSIGN TO W57061D4.                
002804     SKIP2                                                                
005001                                                                          
005101 DATA DIVISION.                                                           
005201                                                                          
005301 FILE SECTION.                                                            
005401     SKIP3                                                                
005501 FD  W57061                                                               
005601     RECORDING       F                                                    
005701     BLOCK CONTAINS  0.                                                   
005801 01  IN-POST.                                                             
005901*    03   -COPY W57060    -L.                                             
006001                                                                          
006002 FD  W5706A                                                               
006003     RECORDING       F                                                    
006004     BLOCK CONTAINS  0.                                                   
006005 01  IN2-POST.                                                            
006006*    03   -COPY W57060    -L.                                             
006007                                                                          
006101 FD  W57062                                                               
006201     RECORDING       F                                                    
006301     BLOCK CONTAINS  0.                                                   
006403*01  POST -COPY WDR801 -PRE  UT1-  -L.                                    
006501                                                                          
006502 FD  W5706B                                                               
006503     RECORDING       F                                                    
006504     BLOCK CONTAINS  0.                                                   
006505*01  POST -COPY WDR801 -PRE  UT2-  -L.                                    
006506                                                                          
008001 WORKING-STORAGE SECTION.                                                 
009001                                                                          
009101 77  IDPGM                       PIC X(8)    VALUE 'W5706100'.            
009201 77  JA                          PIC X       VALUE 'J'.                   
009301 77  NEJ                         PIC X       VALUE 'N'.                   
009401 77  W57061-EOF-SW               PIC X       VALUE 'N'.                   
009501     88  END-OF-W57061                       VALUE 'J'.                   
009502 77  W5706A-EOF-SW               PIC X       VALUE 'N'.                   
009503     88  END-OF-W5706A                       VALUE 'J'.                   
010001                                                                          
013001 01  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
013101 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013201     EJECT                                                                
013301 01  DYNAMISKA-SUBPROGRAM.                                                
013401     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013601     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013701     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013801     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
013901                                                                          
014001*    --- PARAMETRAR TILL ABEND                                            
014101 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
014201 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014301 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
014401     SKIP2                                                                
014501 01  FELTEXT.                                                             
014601     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014701     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014801     EJECT                                                                
014901*    --- PARAMETRAR TILL DATKORT                                          
015001 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
015101                                                                          
015201*01  -COPY WDATKORT                                                       
015301     EJECT                                                                
015401                                                                          
015501*    --- PARAMETRAR TILL POSTSUM                                          
015601*                                                                         
015701*01  -COPY W0005   -PRE  POSTSUM-                                         
015801     EJECT                                                                
015901                                                                          
016001 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
016101*    --- INAREA EKONOMIPOST                                               
016201*01  AREA -COPY W57060  -PRE IN-                                          
016301     EJECT                                                                
016401                                                                          
016402 01  FILLER                      PIC X(24)   VALUE 'IN2-AREA'.            
016403*    --- INAREA EKONOMIPOST                                               
016404*01  AREA -COPY W57060  -PRE IN2-                                         
016405     EJECT                                                                
016406                                                                          
016501 01  FILLER                      PIC X(24)   VALUE 'UT1-AREA'.            
016603*01  AREA -COPY WDR801     -PRE UT1-                                      
016703*    05   -COPY W510EKHA   -PRE UT1- -RED UT1-FIL-WDR801-DATA             
016801     EJECT                                                                
016901                                                                          
016902 01  FILLER                      PIC X(24)   VALUE 'UT2-AREA'.            
016903*01  AREA -COPY WDR801     -PRE UT2-                                      
016904*    05   -COPY W510EKHA   -PRE UT2- -RED UT2-FIL-WDR801-DATA             
016905     EJECT                                                                
016906                                                                          
018301*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018401*                                                                         
018501 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018601                                                                          
020001*    --- STATUS-KOD FRÅN IMS                                              
020101 01  STATUS-WS                   PIC XX.                                  
020201     88  SEGMENT-FINNS                       VALUE '  '.                  
020301     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
020401                                                                          
020501 01  GODK-STATUSKODER.                                                    
020601     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020701     SKIP3                                                                
020801 01  SSA1                        PIC X(64).                               
020901 01  SSA2                        PIC X(64).                               
021001                                                                          
021101*    --- IMS FUNKTIONSKODER                                               
021201*01  -COPY W0003                                                          
021301     EJECT                                                                
021401                                                                          
021501*    ---  DLI INPUT-OUTPUT AREA                                           
021601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLSAPA'.                      
021701 01  DLI-IO-WLSAPA.                                                       
021803*    03  -COPY WDR801                                                     
021903*    05   -COPY W510EKHA   -RED FIL-WDR801-DATA                           
022001     EJECT                                                                
022101                                                                          
022201 LINKAGE SECTION.                                                         
022301 PROCEDURE DIVISION.                                                      
022401 MAIN SECTION.                                                            
022601                                                                          
022701     PERFORM A-INIT                                                       
022801                                                                          
024701     PERFORM S01-LAES-W57061                                              
024801     PERFORM UNTIL END-OF-W57061                                          
024901       PERFORM D1-FLYTTA-POST-TILL-UTAREA1                                
027701       PERFORM S01-LAES-W57061                                            
027800     END-PERFORM                                                          
027900                                                                          
027910     PERFORM S01-LAES-W5706A                                              
027920     PERFORM UNTIL END-OF-W5706A                                          
027930       PERFORM D1-FLYTTA-POST-TILL-UTAREA2                                
027940       PERFORM S01-LAES-W5706A                                            
027950     END-PERFORM                                                          
027960                                                                          
028000     PERFORM Z-FINIT                                                      
028100                                                                          
028200     MOVE ZERO TO RETURN-CODE                                             
028300     GOBACK                                                               
028400     .                                                                    
028500     EJECT                                                                
028600                                                                          
028700 A-INIT SECTION.                                                          
028800                                                                          
028901     OPEN INPUT  W57061                                                   
028902     OPEN INPUT  W5706A                                                   
029001     OPEN OUTPUT W57062                                                   
029002     OPEN OUTPUT W5706B                                                   
029600                                                                          
029700     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
029800                                                                          
029900     MOVE FUNCTION CURRENT-DATE(1:4)   TO DAGENS-AAR                      
030000     MOVE FUNCTION CURRENT-DATE(3:6)   TO DAGENS-DATUM                    
030100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
030200     .                                                                    
030300     EJECT                                                                
030400                                                                          
040901 D1-FLYTTA-POST-TILL-UTAREA1 SECTION.                                     
041101     MOVE IN-EKHT-IDPGM      TO UT1-FIL-IDPGM                             
041205     MOVE IN-EKHT-TIREGDAT   TO UT1-FIL-TIREGDAT                          
041301     MOVE IN-EKHT-TIKLOCK    TO UT1-FIL-TIKLOCK                           
041401     MOVE IN-EKHT-IDSEKVNR   TO UT1-FIL-IDSEKVNR                          
041501     MOVE IN-EKHT-IDCPYTXT   TO UT1-FIL-IDCPYTXT                          
041502     MOVE 'EKHA'             TO UT1-FIL-IDCPYTXT(5:4)                     
041601     MOVE IN-EKHT-BEVAT      TO UT1-EKH-BEVAT                             
041701     MOVE IN-EKHT-DAVERDAT   TO UT1-EKH-DAVERDAT                          
041801     MOVE IN-EKHT-FLLSBOK    TO UT1-EKH-FLLSBOK                           
041901     MOVE IN-EKHT-IDANALYS   TO UT1-EKH-IDANALYS                          
042001     MOVE IN-EKHT-IDARTNR    TO UT1-EKH-IDARTNR                           
042101     MOVE IN-EKHT-IDDC-SEND  TO UT1-EKH-IDDC-SEND                         
042201     MOVE IN-EKHT-IDDC-REC   TO UT1-EKH-IDDC-REC                          
042301     MOVE IN-EKHT-IDDISTR    TO UT1-EKH-IDDISTR                           
042401     MOVE IN-EKHT-IDKONTO    TO UT1-EKH-IDKONTO                           
042501     MOVE IN-EKHT-IDKST      TO UT1-EKH-IDKST                             
042601     MOVE IN-EKHT-IDKUNDNR   TO UT1-EKH-IDKUNDNR                          
042701     MOVE IN-EKHT-IDTRANS    TO UT1-EKH-IDTRANS                           
042801     MOVE IN-EKHT-IDVERGL    TO UT1-EKH-IDVERGL                           
042901     MOVE IN-EKHT-KDANMORS   TO UT1-EKH-KDANMORS                          
043001     MOVE IN-EKHT-KDEKHHT    TO UT1-EKH-KDEKHHT                           
043101     MOVE IN-EKHT-KDEKSHT    TO UT1-EKH-KDEKSHT                           
043201     MOVE IN-EKHT-KDEKNIVA   TO UT1-EKH-KDEKNIVA                          
043301     MOVE IN-EKHT-KDFRAKT    TO UT1-EKH-KDFRAKT                           
043401     MOVE IN-EKHT-KDPRODSL   TO UT1-EKH-KDPRODSL                          
043501     MOVE IN-EKHT-KDPSLLOC   TO UT1-EKH-KDPSLLOC                          
043601     MOVE IN-EKHT-KDVALISO   TO UT1-EKH-KDVALISO                          
043701     MOVE IN-EKHT-KVANTAL    TO UT1-EKH-KVANTAL                           
043801     MOVE IN-EKHT-PRARTNTO   TO UT1-EKH-PRARTNTO                          
043901     MOVE IN-EKHT-PRARTSJK   TO UT1-EKH-PRARTSJK                          
044001     MOVE IN-EKHT-PRHEMTAG   TO UT1-EKH-PRHEMTAG                          
044101     MOVE IN-EKHT-PRARTSTD   TO UT1-EKH-PRARTSTD                          
044201     MOVE IN-EKHT-PRDIRLON   TO UT1-EKH-PRDIRLON                          
044301     MOVE IN-EKHT-PRDMTRL    TO UT1-EKH-PRDMTRL                           
044401     MOVE IN-EKHT-PRINK      TO UT1-EKH-PRINK                             
044501     MOVE IN-EKHT-PRKURS     TO UT1-EKH-PRKURS                            
044601     MOVE IN-EKHT-PRLANDCO   TO UT1-EKH-PRLANDCO                          
044701     MOVE IN-EKHT-PROVRPAL   TO UT1-EKH-PROVRPAL                          
044801     MOVE IN-EKHT-SUBEL      TO UT1-EKH-SUBEL                             
044901     MOVE IN-EKHT-SUVAT      TO UT1-EKH-SUVAT                             
045001     MOVE IN-EKHT-DAAVIDAT   TO UT1-EKH-DAAVIDAT                          
045101     MOVE IN-EKHT-IDAVINR    TO UT1-EKH-IDAVINR                           
045201     MOVE IN-EKHT-IDLEVNR    TO UT1-EKH-IDLEVNR                           
045301     MOVE IN-EKHT-KDAVVTYP   TO UT1-EKH-KDAVVTYP                          
045401     MOVE IN-EKHT-KDRT       TO UT1-EKH-KDRT                              
045501     MOVE IN-EKHT-KVANTMOT   TO UT1-EKH-KVANTMOT                          
045601     MOVE IN-EKHT-KVAVIS     TO UT1-EKH-KVAVIS                            
045701     MOVE IN-EKHT-KDSORT     TO UT1-EKH-KDSORT                            
045801     MOVE IN-EKHT-KDTRADP    TO UT1-EKH-KDTRADP                           
045901     MOVE IN-EKHT-FLOVRLEV   TO UT1-EKH-FLOVRLEV                          
046000     IF IN-EKHT-FLDCET = JA                                               
046101       MOVE JA               TO UT1-EKH-FLDCET                            
046200     ELSE                                                                 
046301       MOVE NEJ              TO UT1-EKH-FLDCET                            
046400     END-IF                                                               
046500     IF IN-EKHT-IDORDNR5 NUMERIC                                          
046601       MOVE IN-EKHT-IDORDNR5 TO UT1-EKH-IDORDNR5                          
046700     ELSE                                                                 
046801       MOVE ZERO             TO UT1-EKH-IDORDNR5                          
046900     END-IF                                                               
047001     MOVE IN-EKHT-IDUSER     TO UT1-EKH-IDUSER                            
047101     MOVE SPACE              TO UT1-EKH-IDREF                             
047201                                UT1-EKH-BEFELSAP                          
047301     MOVE IN-EKHT-IDKUNDRF   TO UT1-EKH-IDKUNDRF                          
047302     MOVE IN-EKHT-IDFAKT-EXP TO UT1-EKH-IDFAKT-EXP                        
047303     MOVE IN-EKHT-CMD        TO UT1-EKH-CMD                               
047401     MOVE 'Y'                TO UT1-EKH-FLKLAR                            
047501     PERFORM S11-SKRIV-W57062                                             
047600     .                                                                    
047700     EJECT                                                                
047800                                                                          
047900 D1-FLYTTA-POST-TILL-UTAREA2 SECTION.                                     
048000     MOVE IN2-EKHT-IDPGM      TO UT2-FIL-IDPGM                            
048100     MOVE IN2-EKHT-TIREGDAT   TO UT2-FIL-TIREGDAT                         
048200     MOVE IN2-EKHT-TIKLOCK    TO UT2-FIL-TIKLOCK                          
048300     MOVE IN2-EKHT-IDSEKVNR   TO UT2-FIL-IDSEKVNR                         
048400     MOVE IN2-EKHT-IDCPYTXT   TO UT2-FIL-IDCPYTXT                         
048500     MOVE IN2-EKHT-BEVAT      TO UT2-EKH-BEVAT                            
048600     MOVE IN2-EKHT-DAVERDAT   TO UT2-EKH-DAVERDAT                         
048700     MOVE IN2-EKHT-FLLSBOK    TO UT2-EKH-FLLSBOK                          
048800     MOVE IN2-EKHT-IDANALYS   TO UT2-EKH-IDANALYS                         
048900     MOVE IN2-EKHT-IDARTNR    TO UT2-EKH-IDARTNR                          
049000     MOVE IN2-EKHT-IDDC-SEND  TO UT2-EKH-IDDC-SEND                        
049100     MOVE IN2-EKHT-IDDC-REC   TO UT2-EKH-IDDC-REC                         
049200     MOVE IN2-EKHT-IDDISTR    TO UT2-EKH-IDDISTR                          
049300     MOVE IN2-EKHT-IDKONTO    TO UT2-EKH-IDKONTO                          
049400     MOVE IN2-EKHT-IDKST      TO UT2-EKH-IDKST                            
049500     MOVE IN2-EKHT-IDKUNDNR   TO UT2-EKH-IDKUNDNR                         
049600     MOVE IN2-EKHT-IDTRANS    TO UT2-EKH-IDTRANS                          
049700     MOVE IN2-EKHT-IDVERGL    TO UT2-EKH-IDVERGL                          
049800     MOVE IN2-EKHT-KDANMORS   TO UT2-EKH-KDANMORS                         
049900     MOVE IN2-EKHT-KDEKHHT    TO UT2-EKH-KDEKHHT                          
050000     MOVE IN2-EKHT-KDEKSHT    TO UT2-EKH-KDEKSHT                          
050100     MOVE IN2-EKHT-KDEKNIVA   TO UT2-EKH-KDEKNIVA                         
050200     MOVE IN2-EKHT-KDFRAKT    TO UT2-EKH-KDFRAKT                          
050300     MOVE IN2-EKHT-KDPRODSL   TO UT2-EKH-KDPRODSL                         
050400     MOVE IN2-EKHT-KDPSLLOC   TO UT2-EKH-KDPSLLOC                         
050500     MOVE IN2-EKHT-KDVALISO   TO UT2-EKH-KDVALISO                         
050600     MOVE IN2-EKHT-KVANTAL    TO UT2-EKH-KVANTAL                          
050700     MOVE IN2-EKHT-PRARTNTO   TO UT2-EKH-PRARTNTO                         
050800     MOVE IN2-EKHT-PRARTSJK   TO UT2-EKH-PRARTSJK                         
050900     MOVE IN2-EKHT-PRHEMTAG   TO UT2-EKH-PRHEMTAG                         
051000     MOVE IN2-EKHT-PRARTSTD   TO UT2-EKH-PRARTSTD                         
051100     MOVE IN2-EKHT-PRDIRLON   TO UT2-EKH-PRDIRLON                         
051200     MOVE IN2-EKHT-PRDMTRL    TO UT2-EKH-PRDMTRL                          
051300     MOVE IN2-EKHT-PRINK      TO UT2-EKH-PRINK                            
051400     MOVE IN2-EKHT-PRKURS     TO UT2-EKH-PRKURS                           
051500     MOVE IN2-EKHT-PRLANDCO   TO UT2-EKH-PRLANDCO                         
051600     MOVE IN2-EKHT-PROVRPAL   TO UT2-EKH-PROVRPAL                         
051700     MOVE IN2-EKHT-SUBEL      TO UT2-EKH-SUBEL                            
051800     MOVE IN2-EKHT-SUVAT      TO UT2-EKH-SUVAT                            
051900     MOVE IN2-EKHT-DAAVIDAT   TO UT2-EKH-DAAVIDAT                         
052000     MOVE IN2-EKHT-IDAVINR    TO UT2-EKH-IDAVINR                          
052100     MOVE IN2-EKHT-IDLEVNR    TO UT2-EKH-IDLEVNR                          
052200     MOVE IN2-EKHT-KDAVVTYP   TO UT2-EKH-KDAVVTYP                         
052300     MOVE IN2-EKHT-KDRT       TO UT2-EKH-KDRT                             
052400     MOVE IN2-EKHT-KVANTMOT   TO UT2-EKH-KVANTMOT                         
052500     MOVE IN2-EKHT-KVAVIS     TO UT2-EKH-KVAVIS                           
052600     MOVE IN2-EKHT-KDSORT     TO UT2-EKH-KDSORT                           
052700     MOVE IN2-EKHT-KDTRADP    TO UT2-EKH-KDTRADP                          
052800     MOVE IN2-EKHT-FLOVRLEV   TO UT2-EKH-FLOVRLEV                         
052900     IF IN2-EKHT-FLDCET = JA                                              
053000       MOVE JA               TO UT2-EKH-FLDCET                            
053100     ELSE                                                                 
053200       MOVE NEJ              TO UT2-EKH-FLDCET                            
053300     END-IF                                                               
053400     IF IN2-EKHT-IDORDNR5 NUMERIC                                         
053500       MOVE IN2-EKHT-IDORDNR5 TO UT2-EKH-IDORDNR5                         
053600     ELSE                                                                 
053700       MOVE ZERO             TO UT2-EKH-IDORDNR5                          
053800     END-IF                                                               
053900     MOVE IN2-EKHT-IDUSER    TO UT2-EKH-IDUSER                            
054000     MOVE SPACE              TO UT2-EKH-IDREF                             
054100                                UT2-EKH-BEFELSAP                          
054200     MOVE IN2-EKHT-IDKUNDRF  TO UT2-EKH-IDKUNDRF                          
054210     MOVE IN2-EKHT-IDFAKT-EXP                                             
054220                             TO UT2-EKH-IDFAKT-EXP                        
054230     MOVE IN2-EKHT-CMD       TO UT2-EKH-CMD                               
054300     MOVE 'Y'                TO UT2-EKH-FLKLAR                            
054400     PERFORM S11-SKRIV-W5706B                                             
054500     .                                                                    
054600     EJECT                                                                
054700                                                                          
066200 Z-FINIT SECTION.                                                         
066301     CLOSE W57061                                                         
066401           W57062                                                         
066402           W5706A                                                         
066403           W5706B                                                         
067000     SKIP2                                                                
067100     MOVE 'S' TO POSTSUM-OPKOD                                            
067200     CALL POSTSUM USING POSTSUM-PARM                                      
067300     .                                                                    
067400     EJECT                                                                
067500                                                                          
067601 S01-LAES-W57061  SECTION.                                                
067701     READ W57061          INTO IN-AREA                                    
067800     AT END                                                               
067900        MOVE HIGH-VALUE   TO IN-AREA                                      
068001        SET END-OF-W57061 TO TRUE                                         
068200     NOT AT END                                                           
068300        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
068401        MOVE 'W57061'     TO POSTSUM-FDNAMN                               
068501        MOVE 'W57061D1'   TO POSTSUM-DDNAMN2                              
068600        CALL POSTSUM USING POSTSUM-PARM                                   
068800     END-READ                                                             
068900     .                                                                    
069000                                                                          
069100 S01-LAES-W5706A  SECTION.                                                
069200     READ W5706A          INTO IN2-AREA                                   
069300     AT END                                                               
069400        MOVE HIGH-VALUE   TO IN2-AREA                                     
069500        SET END-OF-W5706A TO TRUE                                         
069600     NOT AT END                                                           
069700        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
069800        MOVE 'W5706A'     TO POSTSUM-FDNAMN                               
069900        MOVE 'W57061D2'   TO POSTSUM-DDNAMN2                              
070000        CALL POSTSUM USING POSTSUM-PARM                                   
070001     END-READ                                                             
070002     .                                                                    
070003                                                                          
070004 S11-SKRIV-W57062 SECTION.                                                
070106     WRITE UT1-POST  FROM UT1-AREA                                        
070200                                                                          
070301     MOVE 'UT1-'     TO POSTSUM-TRANSTYP                                  
070401     MOVE 'W57062'   TO POSTSUM-FDNAMN                                    
070501     MOVE 'W57061D2' TO POSTSUM-DDNAMN2                                   
070600     CALL POSTSUM USING POSTSUM-PARM                                      
070700     .                                                                    
071701     EJECT                                                                
071702                                                                          
071703 S11-SKRIV-W5706B SECTION.                                                
071704     WRITE UT2-POST  FROM UT2-AREA                                        
071705                                                                          
071706     MOVE 'UT2-'     TO POSTSUM-TRANSTYP                                  
071707     MOVE 'W5706B'   TO POSTSUM-FDNAMN                                    
071708     MOVE 'W5706BD2' TO POSTSUM-DDNAMN2                                   
071709     CALL POSTSUM USING POSTSUM-PARM                                      
071710     .                                                                    
071720     EJECT                                                                
