000010 01  KPS-WKPSAREA.                                                        
000020*                                 PARAMETRAR TILL WKPSKONV FÖR            
000030*                                 ATT TA REDA PÅ                          
000040*                                 VILKET KDPRODSL ETT KONTO HAR           
000050*                                 ELLER                                   
000060*                                 OM ETT KDPRODSL ÄR GODKÄNT              
000070*                                 EXEMPEL PÅ ANROP:                       
000080*                                 MOVE 001 TO KPS-KDCALL                  
000090*                                 MOVE IDLKTO TO KPS-IDLKTO               
000100*                                 CALL WKPSKONV USING                     
000110*                                               KPS-WKPSAREA              
000120*                                                                         
000130*                                 RESULTAT ERHÅLLS I KPS-KDSVAR           
000140*                                 SPACE = SAMTLIGA FÄLT I COPY-           
000150*                                         TEXTEN ÄR UPPDATERADE           
000160*                                  F    = SAMTLIGA FÄLT I COPY-           
000170*                                         TEXTEN ÄR UPPDATERADE           
000180*                                         MEN MED INFORMATION OM          
000190*                                         PRODUKTSLAG 11                  
000200*                                                                         
000210*                                 ELLER                                   
000220*                                 MOVE 002 TO KPS-KDCALL                  
000230*                                 MOVE KDPRODSL TO KPS-KDPRODSL           
000240*                                 CALL WKPSKONV USING                     
000250*                                               KPS-WKPSAREA              
000260*                                                                         
000270*                                 RESULTAT ERHÅLLS I KPS-KDSVAR           
000280*                                 SPACE = SAMTLIGA FÄLT I COPY-           
000290*                                         TEXTEN ÄR UPPDATERADE           
000300*                                         IDLKTO = 0000000                
000310*                                  F    = INGA FÄLT I COPYTEXTEN          
000320*                                         ÄR UPPDATERADE                  
000330     03 KPS-KDCALL           PIC 9(3).                                    
000340*                                 ANROPSTYP                               
000350*                                 CALL TYPE                               
000360     03 KPS-BEPRODSL-SVE     PIC X(15).                                   
000370*                                 PRODUKTSLAGSBENÄMNING                   
000380*                                 TYPE OF ASSORTMENT DESCRIPTION          
000390     03 KPS-BEPRODSL-ENG     PIC X(15).                                   
000400*                                 PRODUKTSLAGSBENÄMNING                   
000410*                                 TYPE OF ASSORTMENT DESCRIPTION          
000420     03 KPS-FLBASLPS         PIC X.                                       
000430*                                 BASLAGER PRODUKTSLAGSFLAGGA             
000440*                                 BASIC STOCK ASSORTMENT FLAG             
000450     03 KPS-FLPRODSL         PIC X.                                       
000460*                                 J = NU GÄLLANDE PRODUKTSLAG             
000470*                                 TYPE OF ASSORTMENT FLAG                 
000480     03 KPS-IDLKTO           PIC 9(7).                                    
000490*                                 LAGERKONTO (FFHHHUU)                    
000500*                                 STOCK ACCOUNT (CCMMMSS)                 
000510     03 KPS-IDPROD           PIC 9(2).                                    
000520*                                 PRODUKTKOD                              
000530     03 KPS-KDPRODSL         PIC 9(2).                                    
000540*                                 PRODUKTSLAG                             
000550*                                 PRODUCT GROUP                           
000560     03 KPS-IDFTG            PIC 9(2).                                    
000570*                                 FÖRETAGSID EKONOM REDOVISNING           
000580*                                 COMPANY IDENTITY ACCOUNTING             
000590     03 KPS-KDSVAR           PIC X.                                       
000600*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
000610*                                 RETURN CODE FROM PROGRAM                
000620*** END COPY WKPSAREA  LENGTH=49                                          
