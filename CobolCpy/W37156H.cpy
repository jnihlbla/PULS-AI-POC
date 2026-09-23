000100 01  W37156H.                                                             
000200*                                 FAKTURANS HUVUDPOST                     
000300     03 IDDC                 PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 IDFAKT               PIC S9(7)           COMP-3.                  
000600*                                 FAKTURANUMMER                           
000700     03 VKORDBTO             PIC S9(6)V9(1)      COMP-3.                  
000800*                                 ORDERVIKT BRUTTO (KG)                   
000900     03 VKORDNTO-FAKT        PIC S9(6)V9(1)      COMP-3.                  
001000*                                 NETTOVIKT FAKTURA                       
001100     03 SUFKTBEL             PIC S9(9)V9(2)      COMP-3.                  
001200*                                 SUMMA FAKTURERAT BELOPP                 
001300     03 TIFAKT               PIC S9(7)           COMP-3.                  
001400*                                 FAKTURERINGSDATUM (≈≈MMDD)              
