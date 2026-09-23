//PC22543J JOB (51091925800,HC2N,01,99,0,,,,0),                                 
//          'MACRO FLAGGOR       ',                                             
//          MSGCLASS=H,                                                         
//          CLASS=N                                                             
/*ROUTE  XEQ  LOCAL                                                             
/*ROUTE PRINT NJOV1                                                             
//*                                                                             
//ICEGENER EXEC PGM=ICEGENER                                                    
//SYSIN     DD  DUMMY                                                           
//SYSPRINT  DD  SYSOUT=*                                                        
//SYSUT2    DD  SYSOUT=A,DEST=RSE69395                                          
//SYSUT1    DD  *                                                               
!L M "FLAGGA"                                                                   
!C                                                                              
!C                                                                              
!K 125                                                                          
!F T S 930 2135 L 1 1 3  "Artikelnummer (P)"                                    
!F T S 725 2130 L 1 1 3  "Antal (Q)"                                            
!F T S 685 1300 L 2 2 5  "%V11"                                                 
!F T S 450 2135 L 1 1 3  "Leverantörsnr (V)"                                    
!F T S 240 2130 L 1 1 3  "Kollilöpnr (S)"                                       
!F T S 980 200 R 8 6 6  "%V1"                                                   
!F T S 630 1400 R  5 4 5  "%V2"                                                 
!F T S 425 1400 R 2 2 5  "%V3"                                                  
!F T S 185 1250 R 3 2 5  "%V4"                                                  
!F T S 20 2130 L 1 1 3  "VCAS Göteborg"                                         
!F T S 898 1082 L 1 1 3  "Datum"                                                
!F T S 750 1082 L 1 1 3  "Vikt netto"                                           
!F T S 603 1080 L 1 1 3  "Partinr"                                              
!F T S 450 1080 L 1 1 3  "Plats"                                                
!F T S 894 685 L 1 1 3  "Område"                                                
!F T S 894 397 L 1 1 3  "Gång"                                                  
!F T S 793 1055 L 4 2 5  "%V5"                                                  
!F T S 645 725 R 4 3 5  "%V6"                                                   
!F T S 498 725 R 2 2 5  "%V7"                                                   
!F T S 500 675 L 13 4 5  "%V8"                                                  
!F T S 500 395 L 8 4 5   "%V9"                                                  
!F T S 105 200 R 14 8 5 "%V10"                                                  
!F C S 775 2110 L 130 3 12  "P%V1"                                              
!F C S 495 2115 L 130 3 12  "Q%V2"                                              
!F C S 285 2110 L 130 3 12  "V%V3"                                              
!F C S 45 2110 L 130 3 12   "S%V4"                                              
!F B S 24 1105 L 904 5                                                          
!F B S 925 1100 L 5 950                                                         
!F B S 760 2140 L 5 1035                                                        
!F B S 270 2140 L 5 1035                                                        
!F B S 480 1100 L 5 950                                                         
!F B S 485 705 L 440 5                                                          
!F B S 830 422 L 95 5                                                           
!F B S 775 1100 L 5 395                                                         
!F B S 628 1100 L 5 395                                                         
!F B S 480 2145 L 5 1040                                                        
!L                                                                              
