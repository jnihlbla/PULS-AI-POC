000010 01  WXTR3B.                                                              
000020*                                 PRIMARY EXTRACT                         
000030*                                                                         
000040*                                 PARTNO INFORMATION                      
000050*                                                                         
000060*                                                                         
000070*                                                                         
000080*                                                                         
000090     03 IDARTNR              PIC S9(9)           COMP-3.                  
000100*                                 PART NUMBER                             
000110     03 SUARTFSG-PER         PIC S9(9)V9(2)      COMP-3.                  
000120*                                 SALES AMOUNT PER PART LAST PERI         
000130*                                 OD                                      
000140     03 SUARTFSG-AAR         PIC S9(9)V9(2)      COMP-3.                  
000150*                                 SALES AMOUNT UP TO NOW THIS YEA         
000160*                                 R                                       
000170     03 SUARTFSG-RAAR-SPEC   PIC S9(9)V9(2)      COMP-3.                  
000180*                                 SALES AMOUNT TO SPECIAL PRICE           
000190*                                                                         
000200*                                 CURRENT ROLLING YEAR                    
000210*                                                                         
000220     03 SUARTFSG-RAAR-RAB    PIC S9(9)V9(2)      COMP-3.                  
000230*                                 SALES AMOUNT TO DISCOUNT/FUNC.G         
000240*                                 RP                                      
000250*                                 CURRENT ROLLING YEAR                    
000260*                                                                         
000270     03 SUARTFSG-RAAR-MAN    PIC S9(9)V9(2)      COMP-3.                  
000280*                                 SALES AMOUNT TO MANUALLY SET PR         
000290*                                 ICE                                     
000300*                                 CURRENT ROLLING YEAR                    
000310*                                                                         
000320     03 SUARTFSG-RAAR-KRE    PIC S9(9)V9(2)      COMP-3.                  
000330*                                 CREDITED SALES AMOUNT PER PART          
000340*                                                                         
000350*                                 CURRENT ROLLING YEAR                    
000360*                                                                         
000370     03 SUARTFSG-FAAR        PIC S9(9)V9(2)      COMP-3.                  
000380*                                 SALES AMOUNT/ART "UP TO NOW" BU         
000390*                                 T                                       
000400*                                 CORRESPONDING PERIOD PREVIOUS Y         
000410*                                 EAR                                     
000420     03 SUARTFSG-RAAR        PIC S9(9)V9(2)      COMP-3.                  
000430*                                 SALES AMOUNT CURRENT ROLLING            
000440*                                 YEAR                                    
000450     03 SUARTFSG-FRAAR       PIC S9(9)V9(2)      COMP-3.                  
000460*                                 SALES AMOUNT/PART PREVIOUS              
000470*                                                                         
000480*                                 ROLLING YEAR                            
000490*                                                                         
000500     03 SULEVANT-PER         PIC S9(9)           COMP-3.                  
000510*                                 SUM DELIVERED ART LAST PERIOD           
000520*                                                                         
000530     03 SULEVANT-AAR         PIC S9(9)           COMP-3.                  
000540*                                 SUM DELIVERED PARTS TILL NOW            
000550*                                                                         
000560*                                 THIS YEAR                               
000570*                                                                         
000580     03 SULEVANT-RAAR-SPEC   PIC S9(9)           COMP-3.                  
000590*                                 SUM DELIV ART TO SPECPRICE              
000600*                                 CURRENT ROLLING YEAR                    
000610     03 SULEVANT-RAAR-RAB    PIC S9(9)           COMP-3.                  
000620*                                 SUM DELIVERED PARTS TO                  
000630*                                                                         
000640*                                 DISCOUNT/FGRP ROLLING YEAR              
000650*                                                                         
000660     03 SULEVANT-RAAR-MAN    PIC S9(9)           COMP-3.                  
000670*                                 SUM DEL ART TO MANUAL PRICE             
000680*                                 CURRENT ROLLING YEAR                    
000690     03 SULEVANT-RAAR-KRE    PIC S9(9)           COMP-3.                  
000700*                                 SUM DELIV. ART TO CREDIT CURREN         
000710*                                 T                                       
000720*                                 ROLLING YEAR                            
000730     03 SULEVANT-FAAR        PIC S9(9)           COMP-3.                  
000740*                                 SUM DELIVERED ART "UP TO NOW" B         
000750*                                 UT                                      
000760*                                 CORRESPONDING PERIOD PREVIOUS Y         
000770*                                 EAR                                     
000780     03 SULEVANT-RAAR        PIC S9(9)           COMP-3.                  
000790*                                 SUM DELIVERED ART ROLLING YEAR          
000800*                                                                         
000810     03 SULEVANT-FRAAR       PIC S9(9)           COMP-3.                  
000820*                                 SUM DELIVERED PARTS                     
000830*                                                                         
000840*                                 PREVIOUS ROLLING YEAR                   
000850*                                                                         
000860     03 SUTOTBV-PER          PIC S9(11)V9(2)     COMP-3.                  
000870*                                 CONTRIBUTION MARGIN LAST PERIOD         
000880     03 SUTOTBV-AAR          PIC S9(11)V9(2)     COMP-3.                  
000890*                                 CONTRIBUTION MARGIN UP TO NOW           
000900     03 SUTOTBV-FAAR         PIC S9(11)V9(2)     COMP-3.                  
000910*                                 CONTRIBUTION MARGIN "TILL NOW"          
000920*                                                                         
000930*                                 CORRESPONDING PERIOD PREVIOUS Y         
000940*                                 EAR                                     
000950     03 SUTOTBV-RAAR         PIC S9(11)V9(2)     COMP-3.                  
000960*                                 CONTRIBUTION MARGIN ROLLING             
000970*                                 YEAR                                    
000980     03 SUTOTBV-FRAAR        PIC S9(11)V9(2)     COMP-3.                  
000990*                                 CONTRIBUTION MARGIN FOR                 
001000*                                 PREVIOUS ROLLING YEAR                   
      *** END COPY WXTR3B      LENGTH=139                                       
