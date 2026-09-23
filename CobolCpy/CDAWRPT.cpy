      * GENERATION OF COBOL HOST STRUCTURE FROM CDAWRPT-TAB                     
        01 CDAWRPT.                                                             
      *              ALL REPORTED COMPANYS SALES VALUES                         
         03 IDLANDX2                          PIC X(2).                         
      *              2-STÄLLIG LANDSBETECKNINGSKOD                              
         03 IDDEALER                          PIC X(6).                         
      *              DEALER KUNDNUMMER                                          
         03 DAFSGVV                           PIC X(6).                         
      *              FÖRSÄLJNINGSVECKA ARTIKEL                                  
         03 DAAARP                            PIC X(6).                         
      *              ÅR - REDOVISNINGSPERIOD (ÅÅÅÅRP)                           
      *              12 PER ÅR                                                  
         03 IDARTNR20                         PIC X(20).                        
      *              20-STÄLLIGT ARTIKELNUMMER FÖR AS400 (VIPS)                 
      *              FORMATET ÄR HÖGERJUSTERAT MED INLEDANDE                    
      *              BLANKTECKEN, OCH UTAN INLEDANDE NOLLOR.                    
         03 KDORDTYP-DEAL                     PIC X(1).                         
      *              ORDERTYP HOS DEALER                                        
         03 KDPRODSL                          PIC S9(3) COMP-3.                 
      *              PRODUKTSLAG                                                
         03 KDPRODSL-IMP                      PIC S9(3) COMP-3.                 
      *              PRODUKTSLAG LOKALT HOS IMPORTÖR                            
         03 IDLEVNR-IMP                       PIC S9(5) COMP-3.                 
      *              LEVERANTÖR ENLIGT AVTAL                                    
         03 IDFKNGRP                          PIC S9(5) COMP-3.                 
      *              FUNKTIONSGRUPP                                             
         03 SUSUGRET                          PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL SUGGESTED RETAIL                                
         03 SURET                             PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL SUGGESTED RETAIL                                
         03 SUDLRNET                          PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL DEALER NET                                      
         03 SULANDCO                          PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL LANDED COST                                     
         03 SUPNET                            PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL PURCHASE NET                                    
         03 SUSTDLC                           PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL STANDARD LANDING COST                           
         03 SUARTSJK-ST                       PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL GÄLLANDE SJÄLVCOST                              
         03 SUARTSTD-ST                       PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL GÄLLANDE STANDARDPRIS                           
         03 SULEVANT                          PIC S9(9) COMP-3.                 
      *              SUMMA LEVERERAT ANTAL                                      
      *              AV 1 ARTIKEL                                               
         03 SUBERNET-STOCK                    PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL DEALER NET                                      
         03 SUBERNET-DAILY                    PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL DEALER NET                                      
         03 SUBERLC-STOCK                     PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL LANDED COST BERÄKNAT                            
         03 SUBERLC-DAILY                     PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL LANDED COST BERÄKNAT                            
         03 SUBERPNP-STOCK                    PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
         03 SUBERPNP-DAILY                    PIC S9(13)V9(2) COMP-3.           
      *              VÄRDE TILL PRUCHASE NET BERÄKNAT                           
         03 FLLOKINK                          PIC X(1).                         
      *              ANGER OM ARTIKELN ÄR LOKALT INKÖPT                         
      *              AV IMPORTÖREN                                              
         03 DADATTID                          PIC X(14).                        
      *                                                                         
      ***  END COPY CDAWRPT-TAB                                                 
