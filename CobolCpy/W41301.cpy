000010 01  W41301.                                                              
000020*                                 PRCTABEL FILE                           
000030     03 IDDC                 PIC X(2).                                    
000040*                                 IDENTIFIERARE LAGER                     
000050*                                 WAREHOUSE IDENTIFIER                    
000060     03 IDPRC.                                                            
000070*                                 PRODUKTIONSKANAL                        
000080*                                 PRODUCTION CHANNEL                      
000090        05 IDPRCBAS          PIC X(3).                                    
000100*                                 PRC-BAS                                 
000110*                                 PRC-BASIC                               
000120        05 IDPRCVAR          PIC X.                                       
000130*                                 PRC-VARIANT                             
000140*                                 PRC-VARIANT                             
000150     03 LOW-VALUE            PIC X.                                       
000160     03 ADLAGOMR             OCCURS 10 TIMES                              
000170                             PIC S9(3)           COMP-3.                  
000180*                                 LAGEROMRÅDE                             
000190*                                 AREA                                    
000200     03 BEPRC                PIC X(15).                                   
000210*                                 PRODUKTIONKANALSNAMN                    
000220*                                 NAME OF THE PRODUCTION CHANNEL          
000230     03 IDPRC-HUV.                                                        
000240*                                 HUVUDPRODUKTIONSKANAL                   
000250*                                 MAIN PRODUCTION CHANNEL                 
000260        05 IDPRCBAS-HUV      PIC X(3).                                    
000270*                                 PRC-BAS                                 
000280*                                 PRC-BASIC                               
000290        05 IDPRCVAR-HUV      PIC X.                                       
000300*                                 PRC-VARIANT                             
000310*                                 PRC-VARIANT                             
000320     03 IDPRC-SUB            OCCURS 10 TIMES.                             
000330*                                 PICKUP PRODUKTIONSKANAL                 
000340*                                 PICKUP PRODUCTION CHANNEL               
000350        05 IDPRCBAS-SUB      PIC X(3).                                    
000360*                                 PRC-BAS                                 
000370*                                 PRC-BASIC                               
000380        05 IDPRCVAR-SUB      PIC X.                                       
000390*                                 PRC-VARIANT                             
000400*                                 PRC-VARIANT                             
000410     03 IDPTIDTAB            PIC 9(2).                                    
000420*                                 PRODUKTIONSTIDTABELLSIDENTITET          
000430*                                 PRODUCTION TIME TABLE IDENT.            
000440     03 KDPRCGRP             PIC X(5).                                    
000450*                                 PRODUKTIONSKANALSGRUPP                  
000460*                                 GROUP OF PRODUCTION CHANNELS            
000470     03 KDPRCTYP             PIC X.                                       
000480*                                 PRODUKTIONSKANALSTYP                    
000490*                                 TYPE OF PRODUCTION CHANNEL              
000500     03 KDPRODKL             PIC X.                                       
000510*                                 PRODUKTIONSKLASS                        
000520*                                 PRODUCTION CLASS                        
000530     03 KVARBTID             PIC S9(2)V9(1)      COMP-3.                  
000540*                                 ANTAL MANTIMMAR                         
000550*                                 NUMBER OF MAN HOURS                     
000560     03 KVBEMAN-ORD          PIC S9(2)V9(1)      COMP-3.                  
000570*                                 BEMANNING, KAPACITET ORDINARIE          
000580*                                 AVAILABLE CAPACITY ORDINARY             
000590     03 KVBEMAN-EXT          PIC S9(2)V9(1)      COMP-3.                  
000600*                                 BEMANNING, KAPACITET (EXTRA)            
000610*                                 AVAILABLE CAPACITY (EXTRA)              
000620     03 KVORDER              PIC S9(7)           COMP-3.                  
000630*                                 ANTAL ORDER                             
000640*                                 QUANTITY OF ORDERS                      
000650     03 KVRADER              PIC S9(5)           COMP-3.                  
000660*                                 ANTAL RADER                             
000670*                                 NUMBER OF LINES                         
000680     03 KVVTID               PIC S9(3)V9(2)      COMP-3.                  
000690*                                 ORDER VÄNTETID I PRC (TTMM)             
000700*                                 WAITING TIME IN PRC (HHMM)              
000710     03 VKORDNTO             PIC S9(6)V9(1)      COMP-3.                  
000720*                                 ORDERVIKT NETTO (KG)                    
000730*                                 WEIGHT PER ORDER NETTO (KG)             
000740     03 VLKOLGR              PIC S9V9(2)         COMP-3.                  
000750*                                 NETTOGRÄNS FÖR EGET KOLLI I M3          
000760*                                 NET LIMIT FOR OWN CASE IN M3            
000770     03 VLORDNTO             PIC S9(4)V9(3)      COMP-3.                  
000780*                                 ORDERVOLYM NETTO (M3)                   
000790*                                 NET VOLUME PER ORDER (M3)               
000800     03 FILLER               PIC X(31).                                   
      *** END COPY W41301      LENGTH=152                                       
