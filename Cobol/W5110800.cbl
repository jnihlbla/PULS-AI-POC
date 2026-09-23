000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W5110800.                                                
000003 AUTHOR.         KARL JOHAN.                                              
000004 DATE-WRITTEN.   SEPT 83.                                                 
000005     REMARKS.                                                             
000006*          SKAPAR LISTA ÖVER ALLA ARTIKLAR PÅ CDC MED SALDO               
000007*     INDATA                                                              
000008*          ARTIKELREGISTER-EXTRAKT W01177                                 
000009*     UTDATA                                                              
000010*          LISTA EN RAD FÖR VARJE ART. PÅ CDC, MED SALDO.                 
000011     EJECT                                                                
000012 ENVIRONMENT DIVISION.                                                    
000013 INPUT-OUTPUT SECTION.                                                    
000014 FILE-CONTROL.                                                            
000015     SKIP2                                                                
000016     SELECT W01177              ASSIGN TO UT-S-W51108D1.                  
000017     SELECT LISTFIL             ASSIGN TO UT-S-W51108D2.                  
000018     SKIP3                                                                
000019 DATA DIVISION.                                                           
000020 FILE SECTION.                                                            
000021                                                                          
000022 FD  W01177                                                               
000023     RECORDING F                                                          
000024     BLOCK CONTAINS 0.                                                    
000025                                                                          
000026*01  -COPY W011100    -L.                                                 
000027                                                                          
000028 FD  LISTFIL                                                              
000029     RECORDING V                                                          
000030     BLOCK CONTAINS 0.                                                    
000031                                                                          
000032 01  LISTPOST                     PIC X(145).                             
000033     EJECT                                                                
000034 WORKING-STORAGE SECTION.                                                 
000035                                                                          
000036 77  IDPGM               PIC X(8)        VALUE 'W5110800'.                
000037 77  JA                  PIC X               VALUE 'J'.                   
000038 77  NEJ                 PIC X               VALUE 'N'.                   
000039 77  FL-SKRIVRAD         PIC X           VALUE 'N'.                       
000040 77  LB-EOF              PIC X           VALUE 'N'.                       
000041 01  RAD-ANT             PIC S9          VALUE +0    COMP-3.              
000042 01  SIDNR               PIC S9(5)       VALUE +0    COMP-3.              
000043 01  RADNR               PIC S9(3)       VALUE +111  COMP-3.              
000044                                                                          
000045 01  SUBPROGRAM.                                                          
000046     03  DATKORT         PIC X(8)        VALUE 'DATKORT'.                 
000047                                                                          
000048 01  WDATUM              PIC X(6)        VALUE 'WDATUM'.                  
000049*01  -COPY WDATKORT.                                                      
000050     EJECT                                                                
000051 01  DLB.                                                                 
000052*    03  -COPY W011100 -PRE LB-.                                          
000053     EJECT                                                                
000054 01  TITEL1.                                                              
000055     03  FILLER              PIC X(17)   VALUE                            
000056         ' VCCS W51108-001 '.                                             
000057     03  FILLER              PIC X(25)   VALUE                            
000058         '     SALDOLISTA CDC      '.                                     
000059     03  TIT1-AR             PIC  99.                                     
000060     03  TIT1-MAN            PIC B99.                                     
000061     03  TIT1-DAG            PIC B99.                                     
000062     03  FILLER              PIC X(11)   VALUE '      ARTNR'.             
000063     03  TIT1-ARTNR          PIC Z(9).                                    
000064     03  FILLER              PIC X(10)   VALUE '     SIDA '.              
000065     03  TIT1-SIDA           PIC Z(3)9.                                   
000066                                                                          
000067 01  RUBRIK1.                                                             
000068     03  FILLER              PIC X(37) VALUE                              
000069         '      ART ERS ART.ADRESS   AKS AK-PÅV'.                         
000070     03  FILLER              PIC X(25) VALUE                              
000071         '      LS    EFR  STD.PRIS'.                                     
000072     03  FILLER              PIC X(11) VALUE ' UR LEVNR  '.               
000073     03  FILLER              PIC X(37) VALUE                              
000074         '      ART ERS ART.ADRESS   AKS AK-PÅV'.                         
000075     03  FILLER              PIC X(25) VALUE                              
000076         '      LS    EFR  STD.PRIS'.                                     
000077     03  FILLER              PIC X(9)  VALUE ' UR LEVNR'.                 
000079     EJECT                                                                
000080 01  DETALJRAD.                                                           
000081     03  RAD-IDARTNR1          PIC Z(8)9.                                 
000082     03  RAD-KDERS1            PIC ZZ9.                                   
000083     03  RAD-ADLAGOMR-BD1      PIC Z99.                                   
000084     03  RAD-KDGANG-BD1        PIC Z99.                                   
000085     03  RAD-ADPLATS-BD1       PIC Z9(5).                                 
000086     03  RAD-KVAKS1            PIC Z(5)9-.                                
000087     03  RAD-KVAKS-PAV1        PIC Z(5)9-.                                
000088     03  RAD-KVLS1             PIC Z(6)9-.                                
000089     03  RAD-KVEFRS1           PIC Z(5)9-.                                
000090     03  RAD-PRARTSTD1         PIC Z(6).99.                               
000091     03  FILLER                PIC X VALUE SPACE.                         
000092     03  RAD-KDARTURS1         PIC X(2).                                  
000093     03  FILLER                PIC X VALUE SPACE.                         
000094     03  RAD-IDLEVNR1          PIC X(5).                                  
000095     03  RAD-IDARTNR2          PIC Z(10)9.                                
000097     03  RAD-KDERS2            PIC ZZ9.                                   
000098     03  RAD-ADLAGOMR-BD2      PIC Z99.                                   
000099     03  RAD-KDGANG-BD2        PIC Z99.                                   
000100     03  RAD-ADPLATS-BD2       PIC Z9(5).                                 
000101     03  RAD-KVAKS2            PIC Z(5)9-.                                
000102     03  RAD-KVAKS-PAV2        PIC Z(5)9-.                                
000103     03  RAD-KVLS2             PIC Z(6)9-.                                
000104     03  RAD-KVEFRS2           PIC Z(5)9-.                                
000105     03  RAD-PRARTSTD2         PIC Z(6).99.                               
000106     03  FILLER                PIC X VALUE SPACE.                         
000107     03  RAD-KDARTURS2         PIC X(2).                                  
000108     03  FILLER                PIC X VALUE SPACE.                         
000109     03  RAD-IDLEVNR2          PIC X(5).                                  
000110     EJECT                                                                
000111 PROCEDURE DIVISION.                                                      
000112                                                                          
000113     PERFORM A-INITIERA                                                   
000114                                                                          
000115     PERFORM S01-LAS-LAGERBANDET                                          
000116                                                                          
000117     PERFORM UNTIL LB-EOF = JA                                            
000118       IF LB-KVEFRS NOT = 0 OR LB-KVAKS-CDC NOT = 0 OR                    
000119          LB-KVAKS-PAV NOT = 0 OR LB-KVLS NOT = 0                         
000120         IF LB-KDERS < 21                                                 
000121           IF FL-SKRIVRAD = NEJ                                           
000122             PERFORM B-REDIGERA-DELRAD1                                   
000123           ELSE                                                           
000124             PERFORM C-REDIGERA-DELRAD2                                   
000125           END-IF                                                         
000126         END-IF                                                           
000127       END-IF                                                             
000128       PERFORM S01-LAS-LAGERBANDET                                        
000129     END-PERFORM                                                          
000130                                                                          
000131     PERFORM Z-AVSLUTA                                                    
000132     MOVE ZERO TO RETURN-CODE                                             
000133     GOBACK                                                               
000134     .                                                                    
000135 A-INITIERA SECTION.                                                      
000136                                                                          
000137     OPEN INPUT W01177                                                    
000138          OUTPUT LISTFIL                                                  
000139     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
000140     MOVE D-AAR             TO TIT1-AR                                    
000141     MOVE D-MAANAD          TO TIT1-MAN                                   
000142     MOVE D-DAG             TO TIT1-DAG                                   
000143     .                                                                    
000144     EJECT                                                                
000145 B-REDIGERA-DELRAD1 SECTION.                                              
000146                                                                          
000147     MOVE LB-IDARTNR        TO RAD-IDARTNR1                               
000148     MOVE LB-KDERS          TO RAD-KDERS1                                 
000149     MOVE LB-ADLAGOMR       TO RAD-ADLAGOMR-BD1                           
000150     MOVE LB-ADGANG         TO RAD-KDGANG-BD1                             
000151     MOVE LB-ADPLATS        TO RAD-ADPLATS-BD1                            
000152     MOVE LB-KVAKS-CDC      TO RAD-KVAKS1                                 
000153     MOVE LB-KVAKS-PAV      TO RAD-KVAKS-PAV1                             
000154     MOVE LB-KVLS           TO RAD-KVLS1                                  
000155     MOVE LB-KVEFRS         TO RAD-KVEFRS1                                
000156     MOVE LB-PRARTSTD       TO RAD-PRARTSTD1                              
000157     MOVE LB-KDARTURS       TO RAD-KDARTURS1                              
000158     MOVE LB-IDLEVNR        TO RAD-IDLEVNR1                               
000159     MOVE JA TO FL-SKRIVRAD                                               
000160     .                                                                    
000161     EJECT                                                                
000162 C-REDIGERA-DELRAD2 SECTION.                                              
000163                                                                          
000164     MOVE LB-IDARTNR        TO RAD-IDARTNR2                               
000165     MOVE LB-KDERS          TO RAD-KDERS2                                 
000166     MOVE LB-ADLAGOMR       TO RAD-ADLAGOMR-BD2                           
000167     MOVE LB-ADGANG         TO RAD-KDGANG-BD2                             
000168     MOVE LB-ADPLATS        TO RAD-ADPLATS-BD2                            
000169     MOVE LB-KVAKS-CDC      TO RAD-KVAKS2                                 
000170     MOVE LB-KVAKS-PAV      TO RAD-KVAKS-PAV2                             
000171     MOVE LB-KVLS           TO RAD-KVLS2                                  
000172     MOVE LB-KVEFRS         TO RAD-KVEFRS2                                
000173     MOVE LB-PRARTSTD       TO RAD-PRARTSTD2                              
000174     MOVE LB-KDARTURS       TO RAD-KDARTURS2                              
000175     MOVE LB-IDLEVNR        TO RAD-IDLEVNR2                               
000176     PERFORM S02-SKRIV-RAD                                                
000177     MOVE NEJ TO FL-SKRIVRAD                                              
000178     .                                                                    
000179     EJECT                                                                
000180 Z-AVSLUTA SECTION.                                                       
000181                                                                          
000182     MOVE SPACE TO LISTPOST                                               
000183     CLOSE LISTFIL W01177                                                 
000184     .                                                                    
000185     EJECT                                                                
000186 S01-LAS-LAGERBANDET SECTION.                                             
000187                                                                          
000188     READ W01177 INTO DLB                                                 
000189        AT END MOVE JA      TO LB-EOF                                     
000190     END-READ                                                             
000191     .                                                                    
000192     EJECT                                                                
000193 S02-SKRIV-RAD SECTION.                                                   
000194                                                                          
000195     ADD +1 TO RADNR                                                      
000196*    IF RADNR > 109                                                       
000197     IF RADNR > 50                                                        
000198       MOVE +3              TO RADNR                                      
000199       ADD  +1              TO SIDNR                                      
000200       MOVE SIDNR           TO TIT1-SIDA                                  
000201       MOVE RAD-IDARTNR1    TO TIT1-ARTNR                                 
000202       WRITE LISTPOST FROM TITEL1 AFTER PAGE                              
000203       WRITE LISTPOST FROM RUBRIK1 AFTER 2                                
000204     END-IF                                                               
000205                                                                          
000210     WRITE LISTPOST FROM DETALJRAD AFTER 1                                
000300     .                                                                    
