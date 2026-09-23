//W551J059 JOB (650W5510100W551J059,W100),'RTN W551B1',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=K                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//     INCLUDE MEMBER=SYST5                                                     
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//W551    EXEC W551P059                                                         
//W55159.DBOCTRL   DD *                                                         
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:43:59                             
                                                                                
  FUNCTION=RELOAD,USERLOAD,                                                     
  PCB=1,                                                                        
  HDSORT=YES,                                                                   
  IIRRECORD=NO                                                                  
                                                                                
/*                                                                              
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDC6V,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDC6ACLU),DISP=SHR                            
//*                                                                             
//WDC6    EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=SHR                                       
//SIU.WDC6AK DD DSN=WG01.QASE.WDC6AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:43:59                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDC6,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOPEND  EXEC WSOPEND,PROCESS=W551J059                                         
//*                                                                             
