//W114Z4SE JOB (640W1140100W114Z4SE,W100),'RTN W114SA',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*                                                                             
//*** DC71 REQ ARTIKLAR TILL SI+                                                
//TOM1    EXEC WEMPTST,DSIN=W114.W114SA.W11476(+0)                              
//VCOM1    EXEC W016P022,VCOM=W114Z1SE,COND=(0,LT,TOM1.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=W114.W114SA.W11476(+0),DISP=SHR                        
//*                                                                             
//*** DC72                                                                      
//TOM2    EXEC WEMPTST,DSIN=W114.W114SA.W11477(+0)                              
//VCOM2    EXEC W016P022,VCOM=W114Z1SE,COND=(0,LT,TOM2.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=W114.W114SA.W11477(+0),DISP=SHR                        
//*                                                                             
//*** DC41                                                                      
//TOM3    EXEC WEMPTST,DSIN=W114.W114SA.W1147A(+0)                              
//VCOM3    EXEC W016P022,VCOM=W114Z1SE,COND=(0,LT,TOM3.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=W114.W114SA.W1147A(+0),DISP=SHR                        
//*                                                                             
//*** DC43                                                                      
//TOM4    EXEC WEMPTST,DSIN=W114.W114SA.W1147B(+0)                              
//VCOM4    EXEC W016P022,VCOM=W114Z1SE,COND=(0,LT,TOM4.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=W114.W114SA.W1147B(+0),DISP=SHR                        
//*                                                                             
//*** DC44                                                                      
//TOM5    EXEC WEMPTST,DSIN=W114.W114SA.W1147C(+0)                              
//VCOM5    EXEC W016P022,VCOM=W114Z1SE,COND=(0,LT,TOM5.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=W114.W114SA.W1147C(+0),DISP=SHR                        
//*                                                                             
//*** DC45                                                                      
//TOM6    EXEC WEMPTST,DSIN=W114.W114SA.W1147D(+0)                              
//VCOM6    EXEC W016P022,VCOM=W114Z1SE,COND=(0,LT,TOM6.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=W114.W114SA.W1147D(+0),DISP=SHR                        
//*                                                                             
//*** DC46                                                                      
//TOM7    EXEC WEMPTST,DSIN=W114.W114SA.W1147E(+0)                              
//VCOM7    EXEC W016P022,VCOM=W114Z1SE,COND=(0,LT,TOM7.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=W114.W114SA.W1147E(+0),DISP=SHR                        
//*** DC47                                                                      
//TOM8    EXEC WEMPTST,DSIN=W114.W114SA.W1147F(+0)                              
//VCOM8    EXEC W016P022,VCOM=W114Z1SE,COND=(0,LT,TOM8.T)                       
//*                                                                             
//W01622.W016ZZD1 DD DSN=W114.W114SA.W1147F(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W114Z4SE                                         
