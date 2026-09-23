//W233Z8SE JOB (640W2330100W233Z8SE,W100),'RTN W233PV',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//*****************************************************                         
//*       W233Z8SE - PV - CDC                                                   
//*****************************************************                         
//TOM1    EXEC WEMPTST,DSIN=W233.W233PV.W23349(+0)                              
//VCOM1   EXEC W016P022,VCOM=W233Z8SE,COND=(0,LT,TOM1.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=W233.W233PV.W23349(+0),DISP=SHR                        
//*                                                                             
//*****************************************************                         
//*       W233Z8SE - PV - CN                                                    
//*****************************************************                         
//TOM2    EXEC WEMPTST,DSIN=W233.W233PV.W23346(+0)                              
//VCOM2   EXEC W016P022,VCOM=W233Z8SE,COND=(0,LT,TOM2.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=W233.W233PV.W23346(+0),DISP=SHR                        
//*                                                                             
//TOM3    EXEC WEMPTST,DSIN=W233.W233PV.W23347(+0)                              
//VCOM3   EXEC W016P022,VCOM=W233Z8SE,COND=(0,LT,TOM3.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=W233.W233PV.W23347(+0),DISP=SHR                        
//*                                                                             
//*TOM4    EXEC WEMPTST,DSIN=W233.W233PV.W23348(+0)                             
//*VCOM4   EXEC W016P022,VCOM=W233Z8SE,COND=(0,LT,TOM4.T)                       
//*                                                                             
//*W01622.W016ZZD1 DD DSN=W233.W233PV.W23348(+0),DISP=SHR                       
//*                                                                             
//*****************************************************                         
//*       W233Z8SE - PV - USA                                                   
//*****************************************************                         
//TOM5    EXEC WEMPTST,DSIN=W233.W233PV.W23350(+0)                              
//VCOM5   EXEC W016P022,VCOM=W233Z8SE,COND=(0,LT,TOM5.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=W233.W233PV.W23350(+0),DISP=SHR                        
//*                                                                             
//TOM6    EXEC WEMPTST,DSIN=W233.W233PV.W23351(+0)                              
//VCOM6   EXEC W016P022,VCOM=W233Z8SE,COND=(0,LT,TOM6.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=W233.W233PV.W23351(+0),DISP=SHR                        
//*                                                                             
//TOM7    EXEC WEMPTST,DSIN=W233.W233PV.W23352(+0)                              
//VCOM7   EXEC W016P022,VCOM=W233Z8SE,COND=(0,LT,TOM7.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=W233.W233PV.W23352(+0),DISP=SHR                        
//*                                                                             
//TOM8    EXEC WEMPTST,DSIN=W233.W233PV.W23353(+0)                              
//VCOM8   EXEC W016P022,VCOM=W233Z8SE,COND=(0,LT,TOM8.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=W233.W233PV.W23353(+0),DISP=SHR                        
//*                                                                             
//TOM9    EXEC WEMPTST,DSIN=W233.W233PV.W23354(+0)                              
//VCOM9   EXEC W016P022,VCOM=W233Z8SE,COND=(0,LT,TOM9.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=W233.W233PV.W23354(+0),DISP=SHR                        
//*                                                                             
//TOMA    EXEC WEMPTST,DSIN=W233.W233PV.W23355(+0)                              
//VCOMA   EXEC W016P022,VCOM=W233Z8SE,COND=(0,LT,TOMA.T)                        
//*                                                                             
//W01622.W016ZZD1 DD DSN=W233.W233PV.W23355(+0),DISP=SHR                        
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W233Z8SE                                         
