//W213J152 JOB (670W2130100W213J152,W100),'RTN W213D3',                         
//             CLASS=V,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV   INCLUDE MEMBER=ENVQASE                                                  
//      INCLUDE MEMBER=SYST2                                                    
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE   PRINT LOCAL                                                           
//*+JBS BIND IMG0                                                               
//*                                                                             
//W213    EXEC W213P052,                                                        
//             INDIN=W213.W213D3.W213522(+0),                                   
//             INDUT=W213.W213D3.W213522(+1)                                    
//*                                                                             
//W21352.W21352D1 DD *                                                          
4149                                                                            
//*                                                                             
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//COPY  EXEC  PGM=V164D0                                                        
//SYSPRINT DD SYSOUT=*                                                          
//SYSIN DD DUMMY,DCB=BLKSIZE=100                                                
//IN DD DSN=W213.W213D3.W213522(+1),DISP=SHR                                    
//UT DD DSN=W213.W213D3.W21352B(+1),DISP=(NEW,CATLG,DELETE),                    
//      DCB=(RECFM=FB,LRECL=12,BLKSIZE=27960),                                  
//      DATACLAS=PSEN,                                                          
//      MGMTCLAS=BACKUPC                                                        
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W213J152                                         
