000010*EDIT ALLOWED                                                             
000100* FIX-COPYTEXT FÖR FIL WB1122  FÖR TILLBEHÖR DB2 TB1ACCE                  
000200*              I RUTIN WB01D1                                             
000300*                                                                         
000310* SAMMA SOM PC0234F2, MED TILLÄGG AV KDPSTA PIC X.                        
000500*                                                                         
000510* SORTAD ASCENDING PÅ                                                     
000511* ARTNR, UPDNR-SU, AONR-T, AOUTG-T, PSLAG, TYPNR, TYPBET                  
000520*                                                                         
000600 01  WB1122.                                                              
000700*                                                                         
000710     03  ARTNR                 PIC 9(8).                                  
000750     03  FKNGRUPPNR            PIC 9(4).                                  
000760     03  POS                   PIC 9(3).                                  
000780     03  ARTTYP                PIC X(4).                                  
000790     03  PSLAG                 PIC X(2).                                  
000791     03  TYPNR                 PIC X(2).                                  
000792     03  TYPBET                PIC X(8).                                  
000793     03  AONR-T                PIC X(6).                                  
000794     03  AOUTG-T               PIC 9(2).                                  
000795     03  AOINFTID6-T           PIC 9(6).                                  
000798     03  ARTNR-OFARGAT         PIC 9(8).                                  
000800     03  FARGSTATUS            PIC X(1).                                  
000820     03  ARTBEN-ENG            PIC X(25).                                 
000830     03  GENANV-ENG            PIC X(25).                                 
000840     03  UPDNR-KU              PIC X(8).                                  
000850     03  UPDNR-SU              PIC X(8).                                  
000870     03  PROJEKT               PIC X(4).                                  
000880     03  NAMN-ANSV-KU          PIC X(25).                                 
000890     03  NAMN-ANSV-SU          PIC X(25).                                 
000891     03  VIKT                  PIC 9(7).                                  
000892     03  MDSMARK               PIC X(1).                                  
000893     03  PSSID                 PIC X(5).                                  
000895     03  ARTUTFGILT            PIC X(8).                                  
000897     03  UPDNR-STATUS          PIC X(6).                                  
000899     03  GSDBNR                PIC X(5).                                  
000910     03  TPD-ST-PH1            PIC X(1).                                  
000930     03  TPD-WEEK-PH1          PIC X(6).                                  
000950     03  QPSW-P-WK-PH1         PIC X(6).                                  
000970     03  QPSW-P-ST-PH1         PIC X(1).                                  
000990     03  QPSW-A-WK-PH1         PIC X(6).                                  
000992     03  QPSW-A-ST-PH1         PIC X(1).                                  
000994     03  PPSW-P-WK-PH2         PIC X(6).                                  
000996     03  PPSW-P-ST-PH2         PIC X(1).                                  
000998     03  PPSW-A-WK-PH2         PIC X(6).                                  
001000     03  PPSW-A-ST-PH2         PIC X(1).                                  
001020     03  CPSW-P-WK-PH3         PIC X(6).                                  
001040     03  CPSW-P-ST-PH3         PIC X(1).                                  
001060     03  CPSW-A-WK-PH3         PIC X(6).                                  
001080     03  CPSW-A-ST-PH3         PIC X(1).                                  
001091     03  FILLER                PIC X(25).                                 
003810     03  KDPSTA                PIC X(1).                                  
003900*** END OF VILMAII-COPY LENGTH= 281                                       
