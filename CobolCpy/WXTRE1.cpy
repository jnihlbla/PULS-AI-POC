000010 01  WXTRE1.                                                              
000020*                                 PRIMÄREXTRAKT                           
000030*                                                                         
000040*                                                                         
000050*                                                                         
000060*                                 LAGEROMRÅDES INFORMATION / ÅÅVV         
000070*                                                                         
000080*                                 AK EFR OKS BELÄGGNINGS-INFO             
000090*                                                                         
000100     03 TIAAVV               PIC S9(5)           COMP-3.                  
000110*                                 ÅR - VECKA  (ÅÅVV)                      
000120     03 KDCLAGER             PIC S9              COMP-3.                  
000130*                                 CENTRALLAGERKOD                         
000140     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
000150*                                 LAGEROMRÅDE                             
000160     03 KDPRODSL             PIC S9(3)           COMP-3.                  
000170*                                 PRODUKTSLAG                             
000180     03 SUSTDTOT-EFR         PIC S9(11)V9(2)     COMP-3.                  
000190*                                 SUMMA VÄRDE TILL STANDARDPRIS           
000200     03 SUSTDTOT-OKS         PIC S9(11)V9(2)     COMP-3.                  
000210*                                 SUMMA VÄRDE TILL STANDARDPRIS           
000220     03 SUSTDTOT-AK          PIC S9(11)V9(2)     COMP-3.                  
000230*                                 SUMMA VÄRDE TILL STANDARDPRIS           
000240     03 SUSTDTOT-LS          PIC S9(11)V9(2)     COMP-3.                  
000250*                                 SUMMA VÄRDE TILL STANDARDPRIS           
000260     03 KVART-LS             PIC S9(7)           COMP-3.                  
000270*                                 ANTAL ARTNR PER BRYTBEGREPP             
000280     03 VLSUMNTO-LS          PIC S9(11)          COMP-3.                  
000290*                                 SUMMERAD NETTOVOLYM (CM3)               
000300     03 VKSUMNTO-LS          PIC S9(11)          COMP-3.                  
000310*                                 SUMMERAD NETTOVIKT (G)                  
      *** END COPY WXTRE1      LENGTH=52                                        
